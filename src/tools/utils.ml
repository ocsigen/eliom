
(** String *)

let remove_spaces s beg endd =
  let rec find_not_space s i step =
    if (i > endd) || (beg > i) then i
    else
      if s.[i] = ' '
      then find_not_space s (i+step) step
      else i
  in
  let first = find_not_space s beg 1 in
  let last = find_not_space s endd (-1) in
  if last >= first
  then String.sub s first (1+ last - first)
  else ""

let rec split c s =
  let longueur = String.length s in
  let rec aux deb =
    if deb >= longueur then []
    else try
      let firstsep = String.index_from s deb c in
      if firstsep = deb then
        aux (deb + 1)
      else
        (remove_spaces s deb (firstsep-1))::
          (aux (firstsep+1))
      with Not_found -> [remove_spaces s deb (longueur-1)]
  in
  aux 0

let chop_extension_if_any name =
  try Filename.chop_extension name with Invalid_argument _ -> name

(** Context *)

let verbose : bool ref = ref false

let args : string list ref = ref []
let package : string list ref = ref []
let predicates : string list ref = ref []
let pp : string option ref = ref None
let kind : [ `Server | `Client | `ServerOpt ] ref = ref `Server
let type_file : string option ref = ref None

let default_server_dir =
  try Sys.getenv "ELIOM_SERVER_DIR"
  with Not_found -> "_server"

let default_client_dir =
  try Sys.getenv "ELIOM_CLIENT_DIR"
  with Not_found -> "_client"

let default_type_dir =
  try Sys.getenv "ELIOM_TYPE_DIR"
  with Not_found -> ""

let build_dir : string ref = ref ""
let type_dir : string ref = ref default_type_dir

(** Findlib *)

let _ = Findlib.init ()

let ext_obj = ".o" (* FIXME WINDOWS *)

let ocamlc = Findlib.command `ocamlc
let ocamlcp = Findlib.command `ocamlcp
let ocamlopt = Findlib.command `ocamlopt
let ocamldep = Findlib.command `ocamldep
let js_of_ocaml = ref "js_of_ocaml"

let compiler = ref ocamlc

let get_predicates () = match !kind with
  | `Server -> ["mt"; "byte"] @ !predicates
  | `Client -> ["byte"] @ !predicates
  | `ServerOpt -> ["native"] @ !predicates

let syntax_predicates = ["preprocessor";"syntax";"camlp4o"] @ !predicates

let get_server_package () =
  Findlib.package_deep_ancestors (get_predicates ()) ("eliom.server" :: !package)
let get_client_package () =
  Findlib.package_deep_ancestors (get_predicates ()) ("eliom.client" :: !package)
let get_syntax_package () =
  Findlib.package_deep_ancestors syntax_predicates ("eliom.syntax" :: !package)

let rec map_include xs = match xs with
  | [] -> []
  | x::xs -> "-I" :: x :: map_include xs

let get_common_include () =
  (match !kind with
  | `Server | `ServerOpt ->
      map_include (List.map Findlib.package_directory (get_server_package ()))
  | `Client ->
      map_include (List.map Findlib.package_directory (get_client_package ())))
  @ match !build_dir with
    | "" | "." -> []
    | d -> ["-I"; d]

let get_common_syntax () =
  map_include (List.map Findlib.package_directory (get_syntax_package ()))
  @ List.concat
    (List.map
       (fun p ->
	 try
	   let objs =
	     Findlib.package_property syntax_predicates p "archive" in
	   List.concat (List.map (split ',') (split ' ' objs))
	 with Not_found -> [])
       (get_syntax_package ()))

let get_client_lib () =
  List.concat
    (List.map
       (fun p ->
	 split ' ' (Findlib.package_property (get_predicates ()) p "archive"))
       (get_client_package ()))

let get_client_js () =
  [ Findlib.package_directory "eliom.client" ^ "/eliom_client.js" ]

let get_pp opt = match !pp with
  | None -> ["-pp"; String.concat " " ("camlp4" :: get_common_syntax () @ opt)]
  | Some pp -> ["-pp"; pp ^ " " ^ String.concat " " (get_common_syntax () @ opt)]

let get_thread_opt () = match !kind with
  | `Client -> []
  | `Server | `ServerOpt -> ["-thread"]

let type_file_suffix = ".type_mli"

let prefix_type_dir name =
  match !type_dir with
  | "" | "." -> name
  | d -> d ^ "/" ^ name

let get_type_file file =
  match !type_file with
  | Some f -> f
  | None ->
      prefix_type_dir (chop_extension_if_any file ^ type_file_suffix)

(** Process *)

let rec wait pid =
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED i -> exit i
  | Unix.WSIGNALED i ->
      Printf.eprintf "Child killed with signal: %d" i;
      exit 255
  | Unix.WSTOPPED i ->
      Printf.eprintf "Child stoped with signal: %d" i;
      wait pid

let create_process
    ?(in_= Unix.stdin) ?(out= Unix.stdout) ?(err= Unix.stderr)
    name args =
  if !verbose then begin
    Printf.eprintf "+ %s" name;
    List.iter (Printf.eprintf " '%s'") args;
    Printf.eprintf "\n%!";
  end;
  Unix.create_process name (Array.of_list (name :: args)) in_ out err

let create_filter name args f =
  let in_, out = Unix.pipe () in
  let pid = create_process ~out name args in
  Unix.close out;
  let ch = Unix.in_channel_of_descr in_ in
  try f ch with _ -> close_in ch; wait pid

let help_filter msg ch =
  ignore (input_line ch);
  ignore (input_line ch);
  prerr_endline msg;
  while true do prerr_endline (input_line ch) done
(** *)

(** *)

let todo () : unit =
  Printf.eprintf "TODO\n%!";
  exit 1
