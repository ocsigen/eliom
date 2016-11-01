open Printf

(* File containing the reserve project names for templates *)
let reserve_project_name_filename = ".eliomreserve"

(* File containing files which must be ignored while copying a template. *)
let eliomignore_filename          = ".eliomignore"

let eliom_template_dir = Findlib.package_directory "eliom.templates"

let distillery_basic = ("basic.ppx", eliom_template_dir)

let template_path (tname, tpath) = tpath ^ "/" ^ tname

(* Returns all lines of [file] as a string list. Returns an empty list if [file]
 * doesn't exist.
 *)
let lines_of_file file =
  if not (Sys.file_exists file) then []
  else
  (
    let lines       = ref [] in
    let in_file     = open_in file in
    let rec aux ()  =
      try lines := (input_line in_file) :: !lines; aux ()
      with End_of_file -> close_in in_file
    in aux (); !lines
  )

let pp_list () l =
  let f s =
    if s = fst distillery_basic
    then s ^ " (default)"
    else s
  in
  String.concat ", " (List.map f l)

let gen_usage_msg l = Printf.sprintf
  "Welcome to the Eliom distillery!\n\
   \n\
   This program generates the scaffold for your Eliom application\n\
   from a template.\n\
   Available templates: %a.\n\
   \n\
   Call it like this\
   \n\  $ %s -name <name> [-template <template>] [-target-directory <dest>]\
   \n\  $ %s -dir \n\
   where"
  pp_list l
  (Filename.basename Sys.argv.(0))
  (Filename.basename Sys.argv.(0))

let rec yes_no : default:bool -> string -> bool =
  fun ~default msg ->
  printf "%s (%s/%s) " msg
    (if default then "YES" else "yes")
    (if default then "no" else "NO");
  match String.(lowercase (read_line ())) with
    | "yes" | "y" -> true
    | "no" | "n" -> false
    | "" -> default
    | _ -> yes_no ~default msg

let split_path str =
  Str.(split_delim (regexp (quote Filename.dir_sep)) str)
let join_path = function
  | [""] -> failwith "join_path"
  | path -> String.concat Filename.dir_sep path

exception Preprocessing_error of string
exception File_error of string

let ifdef_regexp = Str.(regexp ("^ *%%%ifdef +\\([A-Z0-9_]+\\)%%% *$"))
let endif_regexp = Str.(regexp ("^ *%%%endif%%% *$"))

let mkdir_p path_str =
  let rec aux sofar = function
    | [] -> ()
    | snippet :: rest ->
      let sofar' = snippet :: sofar in
      if sofar' <> [""] then
        ( let dir = join_path (List.rev sofar') in
          if Sys.file_exists dir then
            ( if not (Sys.is_directory dir) then
                raise
                  (File_error
                     (sprintf "Cannot create directory %S, it's a file" dir)) )
          else
            Unix.mkdir dir 0o775 );
      aux sofar' rest
  in
  aux [] (split_path path_str)


let copy_file ?(env=[]) ?(preds=[]) src_name dst_name =
  let line_counter = ref 0 in
  let include_line =
    let ifdef_stack = ref [] in
    let preds = List.map (fun pred -> pred, Str.(regexp ("^ *%%%ifdef +"^quote pred^"%%% *$"))) preds in
    fun line ->
      let rec ifdef_pred = function
        | [] -> None
        | (pred, regexp) :: rest ->
          if Str.string_match regexp line 0 then
            Some pred
          else ifdef_pred rest
      in
      match ifdef_pred preds with
        | Some _pred ->
          ifdef_stack := true :: !ifdef_stack;
          false
        | None ->
          if Str.string_match ifdef_regexp line 0 then
            ( ifdef_stack := false :: !ifdef_stack;
              false )
          else if Str.string_match endif_regexp line 0 then
            match !ifdef_stack with
              | _ :: stack ->
                ifdef_stack := stack;
                false
              | [] ->
                raise (Preprocessing_error
                         (Printf.sprintf "Cannot match %%%%endif%%%% in line %i in file %S"
                            !line_counter src_name))
          else
            List.for_all (fun x -> x) !ifdef_stack
  in
  let replace_in_line =
    let replacers =
      let replacer (key, value) =
        Str.(global_replace (regexp (quote ("%%%"^key^"%%%"))) value)
      in
      List.map replacer env
    in
    fun line ->
      List.fold_left (fun line replacer -> replacer line) line replacers
  in
  try
    mkdir_p (Filename.dirname dst_name);
    if not (Sys.file_exists dst_name) ||
       ksprintf (yes_no ~default:false)
         "File %S already exists! Overwrite?" dst_name
    then
      let src = open_in src_name in
      let dst = open_out dst_name in
      begin try while true do
          let line = input_line src in
          incr line_counter;
          if include_line line then
            ( output_string dst (replace_in_line line);
              output_char dst '\n' )
        done with End_of_file -> ()
      end;
      close_in src;
      close_out dst;
      printf "Generated %s.\n%!" dst_name
  with
    | Sys_error _ as exc ->
      eprintf "Error generating %s: %s.\n%!" dst_name
        (Printexc.to_string exc)
    | Preprocessing_error msg ->
      eprintf "%s.\n%!" msg
    | File_error msg ->
      eprintf "%s.\n%!" msg

let create_project ~name ~env ~preds ~source_dir ~destination_dir =
  let eliom_ignore_files =
    lines_of_file (Filename.concat source_dir eliomignore_filename)
  in
  if not (Sys.file_exists destination_dir) then
    ( if ksprintf (yes_no ~default:true) "Destination directory %S doesn't exist. Create it?" destination_dir then
        mkdir_p destination_dir
      else
        exit 1 );
  if not (Sys.is_directory destination_dir) then
    ( eprintf "Destination directory %S is a file!" destination_dir;
      exit 1 );
  Array.iter
    (fun src_file ->
      (* First, we check if the file is in .eliomignore *)
      if List.exists (fun x -> x = src_file) eliom_ignore_files then ()
      else
        let src_path = Filename.concat source_dir src_file in
        let dst_path =
          let dst_file = Str.(global_replace (regexp (quote "PROJECT_NAME")) name src_file) in
          let dst_file_path = Str.(split (regexp "!") dst_file) in
          Filename.concat destination_dir (join_path dst_file_path)
        in
        copy_file ~env ~preds src_path dst_path
    )
    (Sys.readdir source_dir)

let env name =
  let db =
    if Utils.has_package "ocsigenserver.ext.ocsipersist-dbm"
    then "dbm"
    else if Utils.has_package "ocsigenserver.ext.ocsipersist-sqlite"
    then "sqlite"
    else "dbm" in
  [
    "PROJECT_NAME", name;
    "MODULE_NAME", String.capitalize name;
    "PROJECT_DB", db
  ]

let preds () = [
  if Sys.ocaml_version >= "4" then
    "OCAML4"
  else
    "OCAML3"
]


let get_templatedirs () =
  let distillery_path_dirs =
    try
      let paths = Sys.getenv "ELIOM_DISTILLERY_PATH" in
      Str.split (Str.regexp ":") paths
    with
    | Not_found -> []
  in
  eliom_template_dir::distillery_path_dirs

let get_templates () =
  let dirs = List.map (fun d -> (d, Unix.opendir d)) (get_templatedirs ()) in
  let rec aux rl (path, dir) =
    try
      let f = Unix.readdir dir in
      if f = ".." || f = "."
      then aux rl (path, dir)
      else
        aux ((f, path)::rl) (path, dir)
    with
      | End_of_file -> Unix.closedir dir; rl
  in List.concat (List.map (aux []) dirs)


(* ------------------------------------------ *)
(* ---------- Reserve project name ---------- *)

(* Check if the project name is valid for the given template. For example,
 * options is not a valid project name for basic.ppx. It is supposed each
 * template has a file {!reserve_project_name_filename} file containing the
 * list of reserve project name (one name a line).
 *)

let check_reserve_project_name project_name template =
  let file = Filename.concat (template_path template) reserve_project_name_filename
  in
  List.exists (fun x -> x = project_name) (lines_of_file file)

(* ---------- Reserve project name ---------- *)
(* ------------------------------------------ *)

let init_project template name =
  env name,
  preds (),
  template_path template

let compilation_unit_name_regexp =
  Str.regexp "^[A-Za-z][a-zA-Z0-9_']*$"

let main () =
  let dir = ref false in
  let shown = ref false in
  let show_templates () =
    List.iter (fun (name, path) -> Printf.printf "%s [%s]\n" name path) (get_templates ());
    shown := true
  in
  let bad fmt = Printf.ksprintf (fun s -> raise (Arg.Bad s)) fmt in
  let name = ref None in
  let template = ref distillery_basic in
  let templates = get_templates () in
  let usage_msg = gen_usage_msg (List.map fst templates) in
  let select_template s =
    try template := List.find (fun (name, _) -> name = s) templates
    with Not_found -> bad "Not a known template name: %S" s
  in
  let destination_dir = ref None in
  let check_name name =
    if not (Str.string_match compilation_unit_name_regexp name 0) then
      bad "Not a valid compilation unit name: %s" name
  in
  let spec = Arg.(align [
      "-dir", Set dir,
      " Display the template directories (set through $ELIOM_DISTILLERY_PATH)";
      "-name", String (fun s -> check_name s; name := Some s),
      "<name> Name of the project (a valid compilation unit name)";
      "-template", String select_template,
      "<template> The template for the project";
      "-list-templates", Unit show_templates,
      " List all available templates";
      "-target-directory", String (fun s -> destination_dir := Some s),
      "<dir> Generate the project in directory <dir> (the project's name by default)";
  ]) in
  Arg.(parse spec (bad "Don't know what to do with %S") usage_msg);
  if !dir then List.iter (printf "%s\n") (get_templatedirs ())
  else if !shown then ()
  else begin
    let template, name, destination_dir =
      match !template, !name with
      | template, Some name ->
          let dir = match !destination_dir with Some dir -> dir | None -> name in
          template, name, dir
      | _ -> Arg.usage spec usage_msg; exit 1
    in
    if (check_reserve_project_name name template) then
      printf
        "'%s' is not a valid project name for the template '%s'.\n"
        name
        (fst template)
    else
      let env, preds, source_dir = init_project template name in
      create_project ~name ~env ~preds ~source_dir ~destination_dir
  end

let () = main ()
