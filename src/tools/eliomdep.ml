
open Utils

let usage () =
  Printf.eprintf "Usage: %s -server <options> <files>\n" (Filename.basename Sys.argv.(0));
  Printf.eprintf "Usage: %s -client <options> <files>\n" (Filename.basename Sys.argv.(0));
  Printf.eprintf "SPECIFIC OPTIONS:\n%!";
  Printf.eprintf "  -dir <dir>\t\tThe default directory for generated files (default %S or %S)\n"
     default_client_dir default_server_dir;
  Printf.eprintf "  -type-dir <dir>\t\tThe directory for generated type_mli files (default %S)\n"
     default_type_dir;
  Printf.eprintf "  -package <name>\tRefer to package when compiling\n";
  Printf.eprintf "  -ppopt <p>\tAppend option <opt> to preprocessor invocation\n";
  Printf.eprintf "  -predicates <p>\tAdd predicate <p> when resolving package properties\n";
  Printf.eprintf "  -verbose\t\tPrint calls to external commands\n";
  create_filter !compiler ["-help"] (help_filter 2 "STANDARD OPTIONS:");
  exit 1

(** Context *)

let do_dump = ref false

let add_build_dir s =
  if s = ":" then s else
  match !build_dir with
  | "" -> s
  | d -> d ^ "/" ^ s

let add_build_dirs line =
  String.concat " " (List.map add_build_dir (split ' ' line ))

let rec filter_dir ch =
  Printf.printf "%s\n" (add_build_dirs (input_line ch));
  filter_dir ch

let rec filter_type ch =
  match split ':' (input_line ch) with
  | [ file; deps ] ->
      Printf.printf "%s : %s\n" (get_type_file file) (add_build_dirs deps);
      filter_type_bis ch
  | _ -> ()
and filter_type_bis ch =
  let line = input_line ch in
  try ignore (String.index line ':')
  with Not_found ->
    Printf.printf "%s\n" (add_build_dirs line);
    filter_type_bis ch

let execute ?(typ = false) name args = match !kind with
  | `Server -> wait (create_process name args)
  | `Client -> create_filter name args filter_dir
  | _ -> assert false

let compile_intf file =
  create_filter
    !compiler ( "-pp" :: get_pp !ppopt :: !args
		@ ["-intf"; file] )
    filter_dir

let compile_impl file =
  create_filter
    !compiler ( "-pp" :: get_pp !ppopt :: !args
		@ ["-impl"; file] )
    filter_dir

let eliom_synonyms = [ "-ml-synonym"; ".eliom"; "-mli-synonym"; ".eliomi" ]

let compile_server_eliom ~mode file =
  let opt = ["pa_eliom_client_server.cmo"; "-notype"] @ !ppopt @ [impl_intf_opt mode] in
  if !do_dump then begin
    let camlp4, ppopt = get_pp_dump ("-printer" :: "o" :: opt @ [file]) in
    ignore (create_process camlp4 ppopt);
    exit 0
  end;
  create_filter
    !compiler ( "-pp" :: get_pp opt :: eliom_synonyms @ !args
		@ [impl_intf_opt mode; file] )
    filter_dir;
  if mode = `Impl then
    let opt = ["pa_eliom_type_filter.cmo"] @ !ppopt @ [impl_intf_opt mode] in
    create_filter
      !compiler ( "-pp" :: get_pp opt :: eliom_synonyms @ !args
		  @ ["-impl"; file] )
      filter_type

let compile_client_eliom ~mode file =
  let ppopt = ["pa_eliom_client_client.cmo"; "-notype"] @ !ppopt @ [impl_intf_opt mode] in
  if !do_dump then begin
    let camlp4, ppopt = get_pp_dump ("-printer" :: "o" :: ppopt @ [file]) in
    ignore (create_process camlp4 ppopt);
    exit 0
  end;
  create_filter !compiler ( "-pp" :: get_pp ppopt :: eliom_synonyms @ !args
			     @ [impl_intf_opt mode; file] ) filter_dir

let compile_eliom ~mode file =
  let basename = chop_extension_if_any file in
  if mode = `Impl then
    (Printf.printf "%s.cmo : %s\n" (add_build_dir basename) (get_type_file file);
     Printf.printf "%s.cmx : %s\n" (add_build_dir basename) (get_type_file file));
  match !kind with
  | `Server -> compile_server_eliom ~mode file
  | `Client -> compile_client_eliom ~mode file
  | _ -> assert false

let rec process_option () =
  let i = ref 2 in
  while !i < Array.length Sys.argv do
    match Sys.argv.(!i) with
    | "-verbose" -> verbose := true; incr i
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
    | "-dir" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      build_dir := Sys.argv.(!i+1);
      i := !i+2
    | "-type-dir" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      type_dir := Sys.argv.(!i+1);
      i := !i+2
    | "-dump" ->
      do_dump := not !do_dump;
      i := !i+1
    | "-intf" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      compile_eliom ~mode:`Intf Sys.argv.(!i+1);
      i := !i+2
    | "-impl" ->
      if !i+1 >= Array.length Sys.argv then usage ();
      compile_eliom ~mode:`Impl Sys.argv.(!i+1);
      i := !i+2
    | arg when Filename.check_suffix arg ".mli" ->
      compile_intf arg; incr i
    | arg when Filename.check_suffix arg ".ml" ->
      compile_impl arg; incr i
    | arg when Filename.check_suffix arg ".eliom" ->
      compile_eliom ~mode:`Impl arg;
      incr i
    | arg when Filename.check_suffix arg ".eliomi" ->
      compile_eliom ~mode:`Intf arg;
      incr i
    | arg -> args := !args @ [arg]; incr i
  done

let process_kind () =
  if Array.length Sys.argv < 2 then usage ();
  match Sys.argv.(1) with
  | "-server" ->
      build_dir := default_server_dir;
       `Server
  | "-client" ->
      build_dir := default_client_dir;
      `Client
  | _ -> usage ()

let main () =
  compiler := ocamldep;
  kind := process_kind ();
  process_option ()

let _ = main ()
