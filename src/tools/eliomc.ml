
(** TOOO dump intermediate file *)

open Utils

let usage () =
  Printf.eprintf "Usage: %s <options> <files>\n" (Filename.basename Sys.argv.(0));
  Printf.eprintf "SPECIFIC OPTIONS:\n";
  Printf.eprintf "  -dir <dir>\t\tThe default directory for generated files (default %S)\n"
    (if !kind = `Client then default_client_dir else default_server_dir);
  if !kind =  `Server || !kind = `ServerOpt then begin
    Printf.eprintf "  -infer\t\tOnly infer the type of values sent by the server\n";
    Printf.eprintf "  -noinfer\t\tDo not infer the type of values sent by the server\n";
  end else begin
    Printf.eprintf "  -jsopt <opt>\t\tAppend option <opt> to js_of_ocaml invocation\n";
  end;
  Printf.eprintf "  -package <name>\tRefer to package when compiling\n";
  Printf.eprintf "  -predicates <p>\tAdd predicate <p> when resolving package properties\n";
  if !kind = `Client then begin
    Printf.eprintf "  -type <file>\tInfered types for the values sent by the server.\n";
  end;
  create_filter !compiler ["-help"] (help_filter "STANDARD OPTIONS:");
  if !kind = `Client then
    create_filter !js_of_ocaml ["-help"] (help_filter "JS_OF_OCAML OPTIONS:");
  exit 1

(** Context *)

let jsopt : string list ref = ref []
let output_name : string option ref = ref None
let noinfer = ref false

let mode : [ `Link | `Compile | `InferOnly | `Library  | `Pack | `Obj | `Shared ] ref =
  ref `Link

let do_compile () = !mode <> `InferOnly
let do_infer () = not !noinfer
(* let do_dump = ref false *)

let create_process ?in_ ?out ?err name args =
  wait (create_process ?in_ ?out ?err name args)

let rec check_or_create_dir name =
  if name <> "/" then
    try ignore(Unix.stat name) with Unix.Unix_error _ ->
      check_or_create_dir (Filename.dirname name);
      Unix.mkdir name 0o777

let prefix_output_dir name =
  match !build_dir with
    | "" -> name
    | d -> d ^ "/" ^ name

let chop_extension_if_any name =
  try Filename.chop_extension name with Invalid_argument _ -> name

let output_prefix ?(ty = false) name =
  let name =
    match !output_name with
    | None ->
	if !mode = `InferOnly || ty
	then prefix_type_dir name
	else prefix_output_dir name
    | Some n ->
	if !mode = `Compile || !mode = `InferOnly
	then (output_name := None; n)
	else prefix_output_dir name in
  check_or_create_dir (Filename.dirname name);
  chop_extension_if_any name

let set_mode m =
 if !mode =  `Link then begin
   if m = `Shared && !kind <> `ServerOpt then usage ();
   if m = `InferOnly && (!kind <> `Server && !kind <> `ServerOpt) then usage ();
   mode := m
 end else
    match !kind with
      | `ServerOpt ->
	  Printf.eprintf
	    "Please specify at most one of -pack, -a, -shared, -c, -infer, -output-obj\n%!";
	exit 1
      | `Client | `Server ->
	  Printf.eprintf
	    "Please specify at most one of -pack, -a, -c, -infer, -output-obj\n%!";
	exit 1

let get_product_name () = match !output_name with
  | None ->
      Printf.eprintf
	"Please specify the name of the output file, using option -o\n%!";
      exit 1
  | Some name ->
      check_or_create_dir (Filename.dirname name);
      name

let build_library () =
  create_process !compiler ( ["-a" ; "-o"  ; get_product_name () ]
			     @ get_common_include ()
			     @ !args )

let build_pack () =
  create_process !compiler ( [ "-pack" ; "-o"  ; get_product_name () ]
			     @ get_common_include ()
			     @ !args )

let build_obj () =
  create_process !compiler ( ["-output-obj" ; "-o"  ; get_product_name () ]
			     @ get_common_include ()
			     @ !args )

let build_shared () =
  create_process !compiler ( ["-shared" ; "-o"  ; get_product_name () ]
			     @ get_common_include ()
			     @ !args )

let get_pp opt = match !pp with
  | None -> ["-pp"; String.concat " " ("camlp4" :: get_common_syntax () @ opt)]
  | Some pp -> ["-pp"; pp ^ " " ^ String.concat " " (get_common_syntax () @ opt)]

let get_thread_opt () = match !kind with
  | `Client -> []
  | `Server | `ServerOpt -> ["-thread"]

let obj_ext () = if !kind = `ServerOpt then ".cmx" else ".cmo"

let compile_intf file =
  if do_compile () then
    let obj = output_prefix file ^ ".cmi" in
    create_process !compiler ( ["-c" ; "-o" ; obj ] @ get_pp [] @ !args
			       @ get_thread_opt ()
			       @ get_common_include ()
			       @ ["-intf"; file] )

let compile_impl file =
  if do_compile () then
    let obj = output_prefix file ^ obj_ext () in
    create_process !compiler ( ["-c" ; "-o"  ; obj ] @ get_pp [] @ !args
			       @ get_thread_opt ()
			       @ get_common_include ()
			       @ ["-impl"; file] );
    args := !args @ [obj]

let compile_obj file =
  if do_compile () then
    ( create_process !compiler (!args @ [file]);
      args := !args @ [output_prefix file ^ ext_obj] )

let compile_server_type_eliom file =
  if do_infer () then
    let obj = output_prefix ~ty:true file ^ type_file_suffix in
    let ppopt = ["pa_eliom_type_filter.cmo"; "-impl"] in
    let out = Unix.openfile obj [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
    create_process ~out !compiler ( [ "-i" ; "-thread" ]
				    @ get_pp ppopt
				    @ !args
				    @ get_common_include ()
				    @ ["-impl"; file] );
    Unix.close out

let compile_server_eliom file =
  if do_compile () then
    let obj = output_prefix file ^ obj_ext () in
    let ppopt = ["pa_eliom_client_server.cmo"; "-impl"] in
    create_process !compiler ( [ "-c" ; "-thread" ; "-o"  ; obj ]
			       @ get_pp ppopt
			       @ !args
			       @ get_common_include ()
			       @ ["-impl"; file] );
    args := !args @ [obj]

let compile_client_eliom file =
  let obj = output_prefix file ^ ".cmo" in
  let ppopt = ["pa_eliom_client_client.cmo"; "-type" ; get_type_file file; "-impl"] in
  create_process !compiler ( ["-c" ; "-o"  ; obj ]
			     @ get_pp ppopt
			     @ !args
			     @ get_common_include ()
			     @ ["-impl"; file] );
  args := !args @ [obj]

let compile_eliom file = match !kind with
  | `Client ->
    compile_client_eliom file
  | `Server | `ServerOpt ->
    compile_server_eliom file;
    compile_server_type_eliom file

let build_server ?(name = "a.out") () = ()
    (* TODO ? Build a staticaly linked ocsigenserver. *)

let build_client () =
  let name = chop_extension_if_any (get_product_name ()) in
  let exe = prefix_output_dir (Filename.basename name) in
  check_or_create_dir (Filename.dirname exe);
  let js = name ^ ".js" in
  create_process !compiler ( ["-o"  ;  exe ]
			     @ get_common_include ()
			     @ get_client_lib ()
			     @ !args );
  create_process !js_of_ocaml ( ["-o" ; js ]
				@ get_client_js ()
				@ !jsopt
				@ [exe] )

let rec process_option () =
  let i = ref 1 in
  while !i < Array.length Sys.argv do
    match Sys.argv.(!i) with
    | "-help" | "--help" -> usage ()
    | "-i" -> todo ()
    | "-c" -> set_mode `Compile; incr i
    | "-a" -> set_mode `Library; incr i
    | "-pack" -> set_mode `Pack; incr i
    | "-output-obj" -> set_mode `Obj; incr i
    | "-shared" -> set_mode `Shared; incr i
    | "-infer" -> set_mode `InferOnly; incr i
    | "-verbose" -> verbose := true; args := !args @ ["-verbose"] ;incr i
    | "-noinfer" when !kind = `Server || !kind = `ServerOpt ->
	noinfer := true; incr i
    | "-vmthread" -> Printf.eprintf "The -vmthread option isn't supported yet."; exit 1
    | "-o" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      output_name := Some Sys.argv.(!i+1);
      i := !i+2
    | "-dir" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      build_dir := Sys.argv.(!i+1);
      i := !i+2
    | "-type-dir" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      type_dir := Sys.argv.(!i+1);
      i := !i+2
    | "-package" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      package := !package @ split ',' Sys.argv.(!i+1);
      i := !i+2
    | "-predicates" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      predicates := !predicates @ split ',' Sys.argv.(!i+1);
      i := !i+2
    | "-pp" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      pp := Some Sys.argv.(!i+1);
      i := !i+2
    | "-jsopt" ->
      if !kind <> `Client then usage ();
      if !i+1 >= Array.length Sys.argv then usage ();
      jsopt := !jsopt @ [Sys.argv.(!i+1)];
      i := !i+2
    | "-type" ->
      if !kind <> `Client then usage ();
      if !i+1 >= Array.length Sys.argv then usage ();
      type_file := Some Sys.argv.(!i+1);
      i := !i+2
    | "-intf" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      compile_intf Sys.argv.(!i+1);
      i := !i+2
    | "-impl" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      compile_eliom Sys.argv.(!i+1);
      i := !i+2
    | arg when Filename.check_suffix arg ".mli" ->
      compile_intf arg; incr i
    | arg when Filename.check_suffix arg ".ml" ->
      compile_impl arg; incr i
    | arg when Filename.check_suffix arg ".eliom" ->
      compile_eliom arg;
      incr i
    | arg when Filename.check_suffix arg ".c" ->
      compile_obj arg; incr i
    | arg -> args := !args @ [arg]; incr i
  done;
  match !mode with
  | `Library -> build_library ()
  | `Pack -> build_pack ()
  | `Obj -> build_obj ()
  | `Shared -> build_shared ()
  | `Link when !kind = `Client -> build_client ()
  | `Link (* Server and ServerOpt *) -> build_server ?name:(!output_name) ()
  | `Compile | `InferOnly -> ()

let main () =
  let k =
    match Filename.basename Sys.argv.(0) with
      | "eliomopt" ->
	  compiler := ocamlopt;
	  build_dir := default_server_dir;
	  `ServerOpt
      | "eliomcp" ->
	  compiler := ocamlcp;
	  build_dir := default_server_dir;
	  `Server
      | "js_of_eliom" ->
	  compiler := ocamlc;
	  build_dir := default_client_dir;
	  `Client
      | "eliomc" | _ ->
	  compiler := ocamlc;
	  build_dir := default_server_dir;
	  `Server in
  kind := k;
  process_option ()

let _ = main ()
