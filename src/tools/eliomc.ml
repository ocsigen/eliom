
open Utils

let usage () =
  Printf.eprintf "Usage: %s <options> <files>\n" (Filename.basename Sys.argv.(0));
  Printf.eprintf "SPECIFIC OPTIONS:\n";
  Printf.eprintf "  -package <name>\tRefer to package when compiling\n";
  if !kind <> `Client then
    Printf.eprintf "  -infer\t\tOnly infer the type of values sent by the server\n";
  Printf.eprintf "  -dir <dir>\t\tThe directory for generated files (default %S)\n"
    (if !kind = `Client then default_client_dir else default_server_dir);
  Printf.eprintf "  -type-dir <dir>\tThe directory to read server type files from (default %S)\n"
     default_type_dir;
  Printf.eprintf "  -server-types-ext <ext> The extension for server type files (default %S)\n"
    default_server_types_ext;
  if !kind = `Client then
    Printf.eprintf "  -jsopt <opt>\t\tAppend option <opt> to js_of_ocaml invocation\n";
  Printf.eprintf "  -ppopt <p>\t\tAppend option <opt> to preprocessor invocation\n";
  Printf.eprintf "  -predicates <p>\tAdd predicate <p> when resolving package properties\n";
  create_filter !compiler ["-help"] (help_filter 2 "STANDARD OPTIONS:");
  if !kind = `Client then
    create_filter !js_of_ocaml ["-help"] (help_filter 1 "JS_OF_OCAML OPTIONS:");
  exit 1

(** Context *)

let jsopt : string list ref = ref []
let output_name : string option ref = ref None

type mode = [ `Link | `Compile | `Infer | `Library  | `Pack | `Obj | `Shared | `Interface ]

let mode : mode ref = ref `Link

let do_compile () = !mode <> `Infer
let do_interface () = !mode = `Interface
let do_dump = ref false

let create_process ?in_ ?out ?err ?on_error name args =
  wait ?on_error (create_process ?in_ ?out ?err name args)

let rec check_or_create_dir name =
  if name <> "/" then
    try ignore(Unix.stat name) with Unix.Unix_error _ ->
      check_or_create_dir (Filename.dirname name);
      try Unix.mkdir name 0o777 with
        (* this append sometime with // compilation *)
        | Unix.Unix_error (Unix.EEXIST,_,_) -> ()
        | Unix.Unix_error (_,_,_) ->
          Printf.eprintf "Unexpected error while creating directory %S" name

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
	if !mode = `Infer || ty
	then prefix_type_dir name
	else prefix_output_dir name
    | Some n ->
	if !mode = `Compile || !mode = `Infer
	then (output_name := None; n)
	else prefix_output_dir name in
  check_or_create_dir (Filename.dirname name);
  chop_extension_if_any name

let set_mode m =
 if !mode = `Link then
   ( if
       (m = `Shared && !kind <> `ServerOpt) ||
       (m = `Infer && !kind = `Client) ||
       (m = `Interface && !kind = `Client)
     then
       usage ()
     else
       mode := m )
 else
   let args =
     let basic_args = ["-pack"; "-a"; "-c"; "output-obj"] in
     let infer_args = if !kind <> `Client then ["-i"; "-infer"] else [] in
     let shared_args = if !kind <> `ServerOpt then ["-shared"] else [] in
     basic_args @ infer_args @ shared_args
   in
   Printf.eprintf
     "Please specify at most one of %s\n%!"
     (String.concat ", " args);
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

let get_thread_opt () = match !kind with
  | `Client -> []
  | `Server | `ServerOpt -> ["-thread"]

let obj_ext () = if !kind = `ServerOpt then ".cmx" else ".cmo"

(* Process ml and mli files *)

let compile_ocaml ~impl_intf file =
  let obj =
    let ext = match impl_intf with
      | `Impl -> obj_ext ()
      | `Intf -> ".cmi"
    in
    output_prefix file ^ ext in
  create_process !compiler ( ["-c" ;
                              "-o" ; obj ;
                              "-pp"; get_pp [] !ppopt]
                             @ !args
			     @ get_thread_opt ()
			     @ get_common_include ()
			     @ [impl_intf_opt impl_intf; file] )

let output_ocaml_interface file =
    create_process !compiler ( ["-i"; "-pp"; get_pp [] !ppopt] @ !args
                               @ get_common_include ()
                               @ [file] )

let process_ocaml ~impl_intf file =
  if do_compile () then
    compile_ocaml ~impl_intf file;
  if !mode = `Interface then
    output_ocaml_interface file

let compile_obj file =
  if do_compile () then
    ( create_process !compiler (!args @ [file]);
      args := !args @ [output_prefix file ^ ext_obj] )

(* Process eliom and eliomi files *)

let get_ppopts ?kind ~impl_intf file =
  let pkg =
    match get_kind kind with
      | `Client -> ["eliom.syntax.client"]
      | `Server | `ServerOpt -> ["eliom.syntax.server"]
  in
  pkg, type_opt impl_intf file @ !ppopt @ [impl_intf_opt impl_intf]

let compile_server_type_eliom file =
  let obj = output_prefix ~ty:true file ^ !server_types_file_ext
  and ppopts = !ppopt @ ["-impl"] in
  if !do_dump then begin
    let camlp4, ppopt =
      get_pp_dump ["eliom.syntax.type"] ("-printer" :: "o" :: ppopts @ [file]) in
    create_process camlp4 ppopt;
    exit 0
  end;
  let out = Unix.openfile obj [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o666 in
  let on_error _ =
    Unix.close out;
    Sys.remove obj
  in
  create_process ~out ~on_error
    !compiler ( [ "-i" ; "-pp"; get_pp ["eliom.syntax.type"] ppopts]
		@ !args
    		@ get_common_include ()
		@ ["-impl"; file] );
  Unix.close out

let output_eliom_interface ~impl_intf file =
  if !do_dump then begin
    Printf.eprintf "Dump (-dump) not supported for interface inference (-i).";
    exit 1
  end;
  let indent ch =
    try
      while true do
        let line = input_line ch in
        Printf.printf "  %s\n" line
      done
    with End_of_file -> ()
  in
  let args kind =
    let pkg, ppopts = get_ppopts ~kind ~impl_intf file in
    let pp = get_pp pkg ppopts in
    [ "-i" ; "-pp" ; pp; "-intf-suffix"; ".eliomi" ]
    @ !args
    @ get_common_include ~kind ()
    @ [ impl_intf_opt impl_intf; file ]
  in
  Printf.printf "(* WARNING `eliomc -i' generated this pretty ad-hoc - use with care! *)\n";
  Printf.printf "{server{\n";
  create_filter !compiler (args `Server) indent;
  Printf.printf "}}\n";
  Printf.printf "{client{\n";
  create_filter !compiler (args `Client) indent;
  Printf.printf "}}\n"

let compile_eliom ~impl_intf file =
  let obj =
    let ext = match !kind with
      | `Client -> ".cmo"
      | `Server | `ServerOpt -> obj_ext ()
    in
    output_prefix file ^ ext
  in
  let pkg, ppopts = get_ppopts ~impl_intf file in
  if !do_dump then begin
    let camlp4, ppopt = get_pp_dump pkg ("-printer" :: "o" :: ppopts @ [file]) in
    create_process camlp4 ppopt;
    exit 0
  end;
  create_process !compiler ( [ "-c" ;
                               "-o"  ; obj ;
                               "-pp" ; get_pp pkg ppopts;
                               "-intf-suffix"; ".eliomi" ]
                             @ get_thread_opt ()
			     @ !args
			     @ get_common_include ()
			     @ [impl_intf_opt impl_intf; file] );
  args := !args @ [obj]

let process_eliom ~impl_intf file =
  match !mode with
  | `Infer when impl_intf = `Impl ->
      compile_server_type_eliom file
  | `Interface ->
      output_eliom_interface ~impl_intf file
  | _ ->
      compile_eliom ~impl_intf file

let build_server ?(name = "a.out") () =
  fail "Linking eliom server is not yet supported"
  (* TODO ? Build a staticaly linked ocsigenserver. *)

let build_client () =
  let name = chop_extension_if_any (get_product_name ()) in
  let exe = prefix_output_dir (Filename.basename name) in
  check_or_create_dir (Filename.dirname exe);
  let js = name ^ ".js" in
  create_process !compiler ( ["-o"  ;  exe ; "-linkall"]
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
    | "-no-autoload" -> autoload_predef := false; incr i
    | "-type_conv" -> type_conv := true; incr i
    | "-i" -> set_mode `Interface; incr i
    | "-c" -> set_mode `Compile; incr i
    | "-a" -> set_mode `Library; incr i
    | "-pack" -> set_mode `Pack; incr i
    | "-output-obj" -> set_mode `Obj; incr i
    | "-shared" -> set_mode `Shared; incr i
    | "-infer" -> set_mode `Infer; incr i
    | "-verbose" -> verbose := true; args := !args @ ["-verbose"] ;incr i
    | "-vmthread" -> Printf.eprintf "The -vmthread option isn't supported yet."; exit 1
    | "-o" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      output_name := Some Sys.argv.(!i+1);
      i := !i+2
    | "-dump" ->
      do_dump := not !do_dump;
      i := !i+1
    | "-dir" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      build_dir := Sys.argv.(!i+1);
      i := !i+2
    | "-type-dir" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      type_dir := Sys.argv.(!i+1);
      i := !i+2
    | "-server-types-ext" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      server_types_file_ext := Sys.argv.(!i+1);
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
    | "-ppopt" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      ppopt := !ppopt @ [Sys.argv.(!i+1)];
      i := !i+2
    | "-intf" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      process_eliom ~impl_intf:`Intf Sys.argv.(!i+1);
      i := !i+2
    | "-impl" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      process_eliom ~impl_intf:`Impl Sys.argv.(!i+1);
      i := !i+2
    | arg when Filename.check_suffix arg ".mli" ->
      process_ocaml ~impl_intf:`Intf arg;
      incr i
    | arg when Filename.check_suffix arg ".ml" ->
      process_ocaml ~impl_intf:`Impl arg;
      incr i
    | arg when Filename.check_suffix arg ".eliom" ->
      process_eliom ~impl_intf:`Impl arg;
      incr i
    | arg when Filename.check_suffix arg ".eliomi" ->
      process_eliom ~impl_intf:`Intf arg;
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
  | `Compile | `Infer | `Interface -> ()

let main () =
  let cmd = Filename.basename Sys.argv.(0) in
  let cmd =
    try
      let idx = String.index cmd '.' in
      String.sub cmd 0 idx
    with Not_found -> cmd in
  let k =
    match cmd with
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
    | "eliomc" ->
	    compiler := ocamlc;
	    build_dir := default_server_dir;
	    `Server
    | s ->
      Format.eprintf "exec name not recognize %S: fallback to ocamlc@." s;
  	  compiler := ocamlc;
	    build_dir := default_server_dir;
	    `Server
  in
  kind := k;
  process_option ()

let _ = main ()
