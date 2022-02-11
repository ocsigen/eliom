open Utils

let usage () =
  Printf.eprintf "Usage: %s -server <options> <files>\n"
    (Filename.basename Sys.argv.(0));
  Printf.eprintf "Usage: %s -client <options> <files>\n"
    (Filename.basename Sys.argv.(0));
  Printf.eprintf "SPECIFIC OPTIONS:\n%!";
  Printf.eprintf
    "  -dir <dir>\t\tThe default directory for generated files (default %S or %S)\n"
    default_client_dir default_server_dir;
  Printf.eprintf
    "  -type-dir <dir>\tThe directory for generated type_mli files (default %S)\n"
    default_type_dir;
  Printf.eprintf
    "  -eliom-inc <dir>\tAdd <dir> to the list of eliom include directories (prepend eliom build directories)\n";
  Printf.eprintf "  -package <name>\tRefer to package when compiling\n";
  Printf.eprintf
    "  -no-autoload\t\tDo not load commonly used syntax extensions (deriving, lwt, js_of_ocaml, tyxml)\n";
  Printf.eprintf
    "  -ppopt <opt>\t\tAppend option <opt> to preprocessor invocation\n";
  Printf.eprintf "  -ppx";
  Printf.eprintf "\t\t\tUse the PPX Eliom syntax extension (default: Camlp4)\n";
  Printf.eprintf "\t\t\tThe above description only applies to the first\n";
  Printf.eprintf "\t\t\tappearance of -ppx in the list of flags. Subsequent\n";
  Printf.eprintf "\t\t\tappearances require an argument and specify a\n";
  Printf.eprintf "\t\t\tPPX preprocessor to use (see STANDARD OPTIONS).\n";
  Printf.eprintf
    "  -predicates <p>\tAdd predicate <p> when resolving package properties\n";
  Printf.eprintf "  -verbose\t\tPrint calls to external commands\n";
  create_filter !compiler ["-help"] (help_filter 2 "STANDARD OPTIONS:");
  exit 1

(* We use inode for eliom include directories, it's the easier way to
 * detect if two directories are the same *)
let inode_of_dir d = (Unix.stat d).Unix.st_ino

(** Context *)

let do_dump = ref false
let mode : [`Normal | `Sort] ref = ref `Normal
let sort_files = ref []
let eliom_inc_dirs = ref ["."]
let eliom_inc_inodes = ref [inode_of_dir "."]
let do_sort () = !mode = `Sort

let in_an_eliom_inc_dir s =
  List.exists
    (fun d_inode -> inode_of_dir (Filename.dirname s) = d_inode)
    !eliom_inc_inodes

let add_build_dir s =
  if s = ":" || not (in_an_eliom_inc_dir s)
  then s
  else match !build_dir with "" -> s | d -> d ^ "/" ^ s

let add_build_dirs line =
  String.concat " " (List.map add_build_dir (split ' ' line))

let server_type_file_dependencies line =
  match split ':' line with
  | [file; deps] ->
      if Filename.check_suffix file ".cmo"
      then Printf.sprintf "%s : %s" (get_type_file file) (add_build_dirs deps)
      else (* Generate only byte-code dependencies *)
        ""
  | _ -> failwith "add_deps_of_type_mli"

let rec on_each_line f ch =
  (* BB Add option -one-line to the calls to ocamldep when we don't
     support OCaml<4 any more. *)
  let rec aux lines =
    (* May fail only when lines=[], it is then handled by create_filter *)
    let line = input_line ch in
    let max_ix = String.length line - 1 in
    if String.length line > 0 && line.[max_ix] = '\\'
    then
      let line' = String.sub line 0 max_ix in
      aux (line' :: lines)
    else String.concat " " (List.rev (line :: lines))
  in
  let line = f (aux []) in
  if line <> "" then (print_string line; print_newline ());
  on_each_line f ch

let eliom_synonyms = ["-ml-synonym"; ".eliom"; "-mli-synonym"; ".eliomi"]

let compile_intf file =
  create_filter !compiler
    (preprocess_opt ~ocaml:true !ppopt
    @ eliom_synonyms @ !args
    @ map_include !eliom_inc_dirs
    @ ["-intf"; file])
    (on_each_line add_build_dirs)

let compile_impl file =
  create_filter !compiler
    (preprocess_opt ~ocaml:true !ppopt
    @ eliom_synonyms @ !args
    @ map_include !eliom_inc_dirs
    @ ["-impl"; file])
    (on_each_line add_build_dirs)

let server_pp_opt impl_intf =
  let l = ["-notype"] @ !ppopt in
  match !pp_mode with `Ppx -> l | _ -> l @ [impl_intf_opt impl_intf]

let client_pp_opt impl_intf =
  let l = ["-notype"] @ !ppopt in
  match !pp_mode with `Ppx -> l | _ -> l @ [impl_intf_opt impl_intf]

let type_pp_opt impl_intf =
  match !pp_mode with `Ppx -> !ppopt | _ -> !ppopt @ [impl_intf_opt impl_intf]

let compile_server_eliom ~impl_intf file =
  if !do_dump
  then (
    let camlp4, ppopt =
      get_pp_dump [] (("-printer" :: "o" :: server_pp_opt impl_intf) @ [file])
    in
    ignore (create_process camlp4 ppopt);
    exit 0);
  create_filter !compiler
    (eliom_synonyms @ !args
    @ map_include !eliom_inc_dirs
    @ get_common_ppx ~kind:`Server ()
    @ preprocess_opt ~kind:`Server (server_pp_opt impl_intf)
    @ [impl_intf_opt impl_intf; file])
    (on_each_line add_build_dirs)

let compile_type_eliom ~impl_intf file =
  if !do_dump
  then (
    (* Won't run because [compile_server_eliom] is first and exits ... *)
    let camlp4, ppopt =
      get_pp_dump [] (("-printer" :: "o" :: type_pp_opt impl_intf) @ [file])
    in
    ignore (create_process camlp4 ppopt);
    exit 0);
  create_filter !compiler
    (eliom_synonyms @ !args
    @ map_include !eliom_inc_dirs
    @ get_common_ppx ~kind:`Server ()
    @ preprocess_opt ~kind:`Types (type_pp_opt impl_intf)
    @ [impl_intf_opt impl_intf; file])
    (on_each_line server_type_file_dependencies)

let compile_client_eliom ~impl_intf file =
  if !do_dump
  then (
    let camlp4, ppopt =
      get_pp_dump [] (("-printer" :: "o" :: client_pp_opt impl_intf) @ [file])
    in
    ignore (create_process camlp4 ppopt);
    exit 0);
  create_filter !compiler
    (eliom_synonyms @ !args
    @ map_include !eliom_inc_dirs
    @ get_common_ppx ~kind:`Client ()
    @ preprocess_opt ~kind:`Client (client_pp_opt impl_intf)
    @ [impl_intf_opt impl_intf; file])
    (on_each_line add_build_dirs)

let compile_eliom ~impl_intf file =
  let basename = chop_extension_if_any file in
  (match !kind with
  | `Server ->
      compile_server_eliom ~impl_intf file;
      if impl_intf = `Impl then compile_type_eliom ~impl_intf file
  | `Client -> compile_client_eliom ~impl_intf file
  | _ -> assert false);
  if impl_intf = `Impl
  then (
    Printf.printf "%s.cmo : %s\n" (add_build_dir basename) (get_type_file file);
    Printf.printf "%s.cmx : %s\n" (add_build_dir basename) (get_type_file file))

let sort () =
  let ppopt =
    match !kind with
    | `Server | `ServerOpt -> server_pp_opt `Impl
    | `Client -> client_pp_opt `Impl
  in
  wait
    (create_process !compiler
       (("-sort" :: eliom_synonyms)
       @ get_common_ppx ~kind:!kind ()
       @ preprocess_opt ~kind:!kind ppopt
       @ map_include !eliom_inc_dirs
       @ List.(concat (map (fun file -> ["-impl"; file]) !sort_files))));
  0

let process_option () =
  let i = ref 2 and ppx = ref false in
  while !i < Array.length Sys.argv do
    match Sys.argv.(!i) with
    | "-verbose" ->
        verbose := true;
        incr i
    | "-no-autoload" ->
        autoload_predef := false;
        incr i
    | "-sort" ->
        mode := `Sort;
        incr i
    | "-eliom-inc" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        let dir = Sys.argv.(!i + 1) in
        eliom_inc_inodes := inode_of_dir dir :: !eliom_inc_inodes;
        eliom_inc_dirs := dir :: !eliom_inc_dirs;
        i := !i + 2
    | "-package" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        package := !package @ split ',' Sys.argv.(!i + 1);
        i := !i + 2
    | "-predicates" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        predicates := !predicates @ split ',' Sys.argv.(!i + 1);
        i := !i + 2
    | "-pp" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        pp := Some Sys.argv.(!i + 1);
        i := !i + 2
    | "-ppopt" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        ppopt := !ppopt @ [Sys.argv.(!i + 1)];
        i := !i + 2
    | "-dir" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        build_dir := Sys.argv.(!i + 1);
        i := !i + 2
    | "-type-dir" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        type_dir := Sys.argv.(!i + 1);
        i := !i + 2
    | "-dump" ->
        do_dump := not !do_dump;
        i := !i + 1
    | "-ppx" when not !ppx ->
        ppx := true;
        pp_mode := `Ppx;
        i := !i + 1
    | "-intf" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        let arg = Sys.argv.(!i + 1) in
        if not (do_sort ()) then compile_eliom ~impl_intf:`Intf arg;
        i := !i + 2
    | "-impl" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        let arg = Sys.argv.(!i + 1) in
        if do_sort ()
        then sort_files := arg :: !sort_files
        else compile_eliom ~impl_intf:`Impl arg;
        i := !i + 2
    | arg when Filename.check_suffix arg ".mli" ->
        if not (do_sort ()) then compile_intf arg;
        incr i
    | arg when Filename.check_suffix arg ".ml" ->
        if do_sort ()
        then sort_files := arg :: !sort_files
        else compile_impl arg;
        incr i
    | arg when Filename.check_suffix arg ".eliom" ->
        if do_sort ()
        then sort_files := arg :: !sort_files
        else compile_eliom ~impl_intf:`Impl arg;
        incr i
    | arg when Filename.check_suffix arg ".eliomi" ->
        if not (do_sort ()) then compile_eliom ~impl_intf:`Intf arg;
        incr i
    | arg ->
        args := !args @ [arg];
        incr i
  done;
  if do_sort () then exit (sort ())

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
