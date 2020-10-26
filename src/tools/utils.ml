
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

let split c s =
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
type kind = [ `Server | `Client | `ServerOpt ]
let kind : kind ref = ref `Server
let type_file : string option ref = ref None

let autoload_predef = ref true

type pp_mode = [ `Camlp4 | `Ppx ]
let pp_mode : pp_mode ref = ref `Camlp4


let default_server_dir =
  try Sys.getenv "ELIOM_SERVER_DIR"
  with Not_found -> "_server"

let default_client_dir =
  try Sys.getenv "ELIOM_CLIENT_DIR"
  with Not_found -> "_client"

let default_type_dir =
  try Sys.getenv "ELIOM_TYPE_DIR"
  with Not_found -> default_server_dir

let default_server_types_ext =
  try Sys.getenv "ELIOM_SERVER_TYPES_EXT"
  with Not_found -> ".type_mli"

let build_dir : string ref = ref ""
let type_dir : string ref = ref default_type_dir

let get_kind k =
  match k with
  | Some k -> k
  | None -> (!kind : kind :> [> kind])

(** Findlib *)

let _ = Findlib.init ()

let ext_obj = ".o" (* FIXME WINDOWS *)

let ocamlc = Findlib.command `ocamlc
let ocamlcp = Findlib.command `ocamlcp
let ocamlopt = Findlib.command `ocamlopt
let ocamldep = Findlib.command `ocamldep
let ocamldoc = Findlib.command `ocamldoc
let js_of_ocaml = ref "js_of_ocaml"

let compiler = ref ocamlc
let camlp4 = ref "camlp4o"

let ppopt : string list ref = ref []

let get_predicates ?kind:k () = match get_kind k with
  | `Server -> ["mt"; "byte"] @ !predicates
  | `Client -> ["byte"] @ !predicates
  | `ServerOpt -> ["native"] @ !predicates

let syntax_predicates = lazy ([ "preprocessor"; "syntax"; "camlp4o" ] @ !predicates)

(* compute pkg_`package_name` predicate like ocamlfind binary does *)
let get_pkg_predicates pkgs =
  List.map
    (fun p -> "pkg_"^p)
    (Findlib.package_deep_ancestors (Lazy.force syntax_predicates) pkgs)

let with_autoload all_pkgs =
  if !autoload_predef
  then begin
    (* Format.eprintf "\nAUTOLOADING PREDEF PKGS\n%s\n@." (String.concat ", " all_pkgs); *)
    "eliom.syntax.predef"::all_pkgs
  end
  else all_pkgs

let get_server_package ?kind:k ?package:p () =
  let package =
    match p with
    | Some p -> p
    | None -> !package
  in
  let pkgs = "eliom.server" :: package in
  let pkg_predicates = get_pkg_predicates pkgs in
  try
    Findlib.package_deep_ancestors
      (get_predicates ?kind:k () @ pkg_predicates)
      pkgs
  with Findlib.No_such_package (name, _) ->
    Printf.eprintf "Unknown package: %s\n%!" name;
    exit 1

let get_client_package ?kind:k () =
  let pkgs = "eliom.client" :: !package in
  let pkg_predicates = get_pkg_predicates pkgs in
  try
    Findlib.package_deep_ancestors
      (get_predicates ?kind:k () @ pkg_predicates)
      pkgs
  with Findlib.No_such_package (name, _) ->
    Printf.eprintf "Unknown package: %s\n%!" name;
    exit 1

let get_syntax_package pkg =
  let resolve_syntax_packages pkgs =
    (* Format.eprintf "pkgs: %s@." (String.concat ", " pkgs); *)
    let pkg_predicates = get_pkg_predicates pkgs in
    try
      Findlib.package_deep_ancestors (Lazy.force syntax_predicates @ pkg_predicates )
        (List.filter
           (fun p ->
              let all_predicates = pkg_predicates @ (Lazy.force syntax_predicates) in
              try
                let objs =
                  Findlib.package_property all_predicates  p "archive" in
                List.concat (List.map (split ',') (split ' ' objs)) <> []
              with Not_found -> false)
           pkgs)
    with Findlib.No_such_package (name, _) ->
      Printf.eprintf "Unknown package: %s\n%!" name;
      exit 1 in
  let all_pkgs = pkg @ !package in
  resolve_syntax_packages (with_autoload all_pkgs)

let has_package name =
  try
    ignore(Findlib.package_directory name);true
  with Findlib.No_such_package (_, _) -> false

let get_ppxs l =
  let meta_ppx_opts =
    List.concat
      (List.map
         (fun pname ->
            try
              let opts = Findlib.package_property [] pname "ppxopt" in
              List.concat
                (List.map
                   (fun opts ->
                     match split ',' opts with
                     | pkg :: opts ->
                       [pkg, (pname, opts)]
                     | [] ->
                       [])
                   (split ' ' opts))
            with Not_found ->
              [])
         l)
  in

  let f p acc =
    let d = Findlib.package_directory p in
    try
      let ppx = Findlib.package_property [] p "ppx" in
      let ppx = Findlib.resolve_path ~base:d ~explicit:true ppx in
      let options =
        List.concat
          (List.map
             (fun (_, (pname, opts)) ->
                let base = Findlib.package_directory pname in
                List.map (Findlib.resolve_path ~base ~explicit:true) opts)
             (List.filter (fun (p', _) -> p' = p) meta_ppx_opts))
      in
      "-ppx" :: String.concat " " (ppx :: options) :: acc
    with Not_found -> acc
  in
  List.fold_right f l []

let get_common_ppx ?kind ?package () =
  get_ppxs @@ match get_kind kind with
  | `Server | `ServerOpt ->
    get_server_package ?kind ?package ()
  | `Client ->
    get_client_package ?kind ()

let rec map_include xs = match xs with
  | [] -> []
  | x::xs -> "-I" :: x :: map_include xs

let get_common_include ?kind:k ?build_dir:dir ?package:p () =
  let dir = match dir with Some d -> d | None -> !build_dir in
  (match get_kind k with
   | `Server | `ServerOpt ->
     "js_of_ocaml" :: get_server_package ?kind:k ?package:p ()
     |> List.map Findlib.package_directory
     |> map_include
   | `Client ->
     map_include (List.map Findlib.package_directory (get_client_package ?kind:k ())))
  @ match dir with
  | "" | "." -> []
  | d -> ["-I"; d]

let get_common_syntax pkg =
  let syntax_pkg = get_syntax_package pkg in
  (* Format.eprintf "pkgs: %s@." (String.concat ", " syntax_pkg); *)
  map_include (List.map Findlib.package_directory syntax_pkg)
  @ List.concat
    (List.map
       (fun p ->
          try
            let objs =
              Findlib.package_property ("byte"::(Lazy.force syntax_predicates)) p "archive" in
            List.concat (List.map (split ',') (split ' ' objs))
          with Not_found -> [])
       syntax_pkg)

let get_client_lib ?kind:k () =
  List.concat
    (List.map
       (fun p ->
          try
            split ' ' (Findlib.package_property (get_predicates ?kind:k ()) p "archive")
          with Not_found -> [])
       (get_client_package ?kind:k ()))

let get_client_js () =
  [
    "+eliom.client/eliom_client.js" ;
  ]

(* Should be called only with -dump... *)
let get_pp_dump pkg opt = match !pp with
  | None -> (!camlp4, get_common_syntax pkg @ opt)
  | Some pp ->
    try
      ignore(String.index pp ' ');
      Printf.eprintf "Incompatible option: -pp and -dump\n%!";
      exit 1
    with Not_found -> (pp, get_common_syntax pkg @ opt)

let get_pp pkg =
  let s = match !pp with
    | None -> String.concat " " (!camlp4 :: get_common_syntax pkg)
    | Some pp -> pp ^ " " ^ String.concat " " (get_common_syntax pkg)
    (* Format.eprintf "get_pp %S@." s *)
  in s

let get_ppx pkg =
  let base = Findlib.package_directory pkg in
  let ppx = Findlib.package_property [] pkg "ppx" in
  Findlib.resolve_path ~base ~explicit:true ppx

let get_thread_opt () = match !kind with
  | `Client -> []
  | `Server | `ServerOpt -> ["-thread"]

let server_types_file_ext = ref default_server_types_ext

let prefix_type_dir name =
  match !type_dir with
  | "" | "." -> name
  | d -> d ^ "/" ^ name

let get_type_file file =
  match !type_file with
  | Some f -> f
  | None ->
    prefix_type_dir (chop_extension_if_any file ^ !server_types_file_ext)

let impl_intf_opt = function
  | `Impl -> "-impl"
  | `Intf -> "-intf"

let type_opt impl_intf file =
  match impl_intf with
  | `Impl -> ["-type"; get_type_file file]
  | `Intf -> ["-notype"]

let simplify_kind ?kind () =
  match get_kind kind with
  | `Client -> `Client
  | `Server | `ServerOpt -> `Server
  | `Types -> `Types

let get_ppopts ~impl_intf file =
  match !pp_mode with
  | `Camlp4 ->
    type_opt impl_intf file @ !ppopt @ [impl_intf_opt impl_intf]
  | `Ppx ->
    type_opt impl_intf file @ !ppopt

let preprocess_opt ?(ocaml = false) ?kind opts =
  match !pp_mode with
  | `Camlp4 ->
    [ "-pp"; get_pp [] ^ " " ^ String.concat " " opts ]
  | `Ppx when ocaml ->
    []
  | `Ppx ->
    let pkg = match simplify_kind ?kind () with
      | `Client -> "eliom.ppx.client"
      | `Server -> "eliom.ppx.server"
      | `Types  -> "eliom.ppx.type"
    in
    [ "-ppx"; get_ppx pkg ^ " " ^ String.concat " " opts ]

(** Process *)

let rec wait ?(on_error=fun _ -> ()) pid =
  let e = snd (Unix.waitpid [] pid) in
  match e with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED i -> on_error e; exit i
  | Unix.WSIGNALED i ->
    Printf.eprintf "Child killed with signal: %d" i;
    on_error e;
    exit 255
  | Unix.WSTOPPED i ->
    Printf.eprintf "Child stopped with signal: %d" i;
    on_error e;
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

let help_filter skip msg ch =
  for _i = 1 to skip do ignore (input_line ch) done;
  prerr_endline msg;
  while true do prerr_endline (input_line ch) done

let fail fmt =
  Printf.ksprintf
    (fun msg ->
       prerr_endline msg;
       exit 1)
    fmt

(** *)

(** *)

let todo () : unit =
  Printf.eprintf "TODO\n%!";
  exit 1
