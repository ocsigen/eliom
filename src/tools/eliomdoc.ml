open Utils

let usage () =
  Printf.eprintf "Usage: %s -server <options> <files>\n"
    (Filename.basename Sys.argv.(0));
  Printf.eprintf "Usage: %s -client <options> <files>\n"
    (Filename.basename Sys.argv.(0));
  Printf.eprintf "SPECIFIC OPTIONS:\n%!";
  Printf.eprintf
    "  -eliom-inc <dir>\tAdd <dir> to the list of eliom include directories (prepend eliom build directories)\n";
  Printf.eprintf "  -package <name>\tRefer to package when compiling\n";
  Printf.eprintf
    "  -no-autoload\t\tDo not load commonly used syntax extensions (deriving, lwt, js_of_ocaml, tyxml)\n";
  Printf.eprintf
    "  -ppopt <p>\t\tAppend option <opt> to preprocessor invocation\n";
  Printf.eprintf
    "  -predicates <p>\tAdd predicate <p> when resolving package properties\n";
  Printf.eprintf "  -verbose\t\tPrint calls to external commands\n";
  Printf.eprintf "  -ppx";
  Printf.eprintf "\t\t\tUse the PPX Eliom syntax extension (default: Camlp4)\n";
  Printf.eprintf "\t\t\tThe above description only applies to the first\n";
  Printf.eprintf "\t\t\tappearance of -ppx in the list of flags. Subsequent\n";
  Printf.eprintf "\t\t\tappearances require an argument and specify a\n";
  Printf.eprintf "\t\t\tPPX preprocessor to use (see STANDARD OPTIONS).\n";
  create_filter !compiler ["-help"] (help_filter 2 "STANDARD OPTIONS:");
  exit 1

(* We use inode for eliom include directories, it's the easier way to
 * detect if two directories are the same *)
let inode_of_dir d = (Unix.stat d).Unix.st_ino

(** Context *)

let acc_file = ref None

let get_default_args () =
  let new_ = Filename.temp_file "eliomdoc" "dump" in
  let args =
    match !acc_file with
    | None -> ["-dump"; new_]
    | Some old -> ["-load"; old; "-dump"; new_]
  in
  acc_file := Some new_;
  args

let eliom_inc_dirs = ref []
let eliom_inc_inodes = ref []

let in_an_eliom_inc_dir s =
  List.exists
    (fun d_inode -> inode_of_dir (Filename.dirname s) = d_inode)
    !eliom_inc_inodes

let compile_intf file =
  wait
    (create_process !compiler
       (preprocess_opt !ppopt @ !args @ get_default_args ()
      @ get_common_include ()
       @ map_include !eliom_inc_dirs
       @ ["-intf"; file]))

let compile_impl file =
  wait
    (create_process !compiler
       (preprocess_opt !ppopt @ !args @ get_default_args ()
      @ get_common_include ()
       @ map_include !eliom_inc_dirs
       @ ["-impl"; file]))

let server_pp_opt impl_intf =
  match !pp_mode with
  | `Camlp4 -> ("-printer" :: "o" :: !ppopt) @ [impl_intf_opt impl_intf]
  | `Ppx -> !ppopt

let client_pp_opt impl_intf =
  match !pp_mode with
  | `Camlp4 -> ("-printer" :: "o" :: !ppopt) @ [impl_intf_opt impl_intf]
  | `Ppx -> !ppopt

let generate_temp_file file =
  let tmp_dir = Filename.get_temp_dir_name () in
  let temp_file = tmp_dir ^ Filename.dir_sep ^ Filename.basename file in
  ( temp_file
  , Unix.openfile temp_file [Unix.O_TRUNC; Unix.O_CREAT; Unix.O_WRONLY] 0o640 )

let compile_server_eliom ~impl_intf file =
  let file', out = generate_temp_file file in
  wait (create_process ~out "eliompp" ["-server"; file]);
  wait
    (create_process !compiler
       (preprocess_opt ~kind:`Server (server_pp_opt impl_intf)
       @ !args @ get_default_args () @ get_common_include ()
       @ map_include !eliom_inc_dirs
       @ [impl_intf_opt impl_intf; file']))

let compile_client_eliom ~impl_intf file =
  let file', out = generate_temp_file file in
  wait (create_process ~out "eliompp" ["-client"; file]);
  wait
    (create_process !compiler
       (preprocess_opt ~kind:`Client (client_pp_opt impl_intf)
       @ !args @ get_default_args () @ get_common_include ()
       @ map_include !eliom_inc_dirs
       @ [impl_intf_opt impl_intf; file']))

let compile_eliom ~impl_intf file =
  match !kind with
  | `Server -> compile_server_eliom ~impl_intf file
  | `Client -> compile_client_eliom ~impl_intf file
  | _ -> assert false

let generate_doc () =
  match !acc_file with
  | None -> ()
  | Some file -> wait (create_process !compiler ("-load" :: file :: !args))

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
    | "-ppx" when not !ppx ->
        ppx := true;
        pp_mode := `Ppx;
        i := !i + 1
    | "-ppopt" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        ppopt := !ppopt @ [Sys.argv.(!i + 1)];
        i := !i + 2
    | "-dir" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        build_dir := Sys.argv.(!i + 1);
        i := !i + 2
    | "-intf" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        let arg = Sys.argv.(!i + 1) in
        compile_eliom ~impl_intf:`Intf arg;
        i := !i + 2
    | "-impl" ->
        if !i + 1 >= Array.length Sys.argv then usage ();
        let arg = Sys.argv.(!i + 1) in
        compile_eliom ~impl_intf:`Impl arg;
        i := !i + 2
    | arg when Filename.check_suffix arg ".mli" -> compile_intf arg; incr i
    | arg when Filename.check_suffix arg ".ml" -> compile_impl arg; incr i
    | arg when Filename.check_suffix arg ".eliom" ->
        compile_eliom ~impl_intf:`Impl arg;
        incr i
    | arg when Filename.check_suffix arg ".eliomi" ->
        compile_eliom ~impl_intf:`Intf arg;
        incr i
    | arg ->
        args := !args @ [arg];
        incr i
  done;
  generate_doc ()

let process_kind () =
  if Array.length Sys.argv < 2 then usage ();
  match Sys.argv.(1) with
  | "-server" -> `Server
  | "-client" -> `Client
  | _ -> usage ()

let main () =
  compiler := ocamldoc;
  kind := process_kind ();
  process_option ()

let _ = main ()
