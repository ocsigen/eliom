
open Printf

let usage_msg = Printf.sprintf
  "Welcome to the Eliom distillery!\n\
   \n\
   This program generates the scaffold for your Eliom application\n\
   from a template. Currently, it only supports a very simple\n\
   template named \"basic\", but later versions will include more\n\
   comprehensive templates!\n\
   \n\
   Call it like this\n  $ %s -name <name> [-template basic] [-destination <dest>]\n\
   where"
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
        | Some pred ->
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
  if not (Sys.file_exists destination_dir) then
    ( if ksprintf (yes_no ~default:true) "Destination directory %S doesn't exists. Create it?" destination_dir then
        mkdir_p destination_dir
      else
        exit 1 );
  if not (Sys.is_directory destination_dir) then
    ( eprintf "Destination directory %S is a file!" destination_dir;
      exit 1 );
  Array.iter
    (fun src_file ->
      let src_path = Filename.concat source_dir src_file in
      let dst_path =
        let dst_file = Str.(global_replace (regexp (quote "PROJECT_NAME")) name src_file) in
        let dst_file_path = Str.(split (regexp "!") dst_file) in
        Filename.concat destination_dir (join_path dst_file_path)
      in
      copy_file ~env ~preds src_path dst_path)
    (Sys.readdir source_dir)

let env name = [
  "PROJECT_NAME", name;
  "MODULE_NAME", String.capitalize name;
]

let preds () = [
  if Sys.ocaml_version >= "4" then
    "OCAML4"
  else
    "OCAML3"
]

let get_datadir () =
  try Sys.getenv "ELIOM_DATA_DIR"
  with Not_found -> Config.datadir

let get_templatedir () =
  Filename.concat (get_datadir ()) Config.templatedir

let get_templates () =
  let dir = Unix.opendir (get_templatedir ()) in
  let rec aux rl =
    try
      let f = Unix.readdir dir in
      if f = ".." || f = "."
      then aux rl
      else aux (f::rl)
    with
      | End_of_file -> Unix.closedir dir; rl
  in aux []

let init_project template name =
  env name,
  preds (),
  Filename.concat (get_templatedir ()) template

let compilation_unit_name_regexp =
  Str.regexp "^[A-Za-z][a-zA-Z0-9_']*$"

let main () =
  let template, name, destination_dir =
    let bad fmt = Printf.ksprintf (fun s -> raise (Arg.Bad s)) fmt in
    let name = ref None in
    let template = ref Config.distillery_basic in
    let templates = get_templates () in
    let select_template s =
      try template := (List.find ((=) s) templates)
      with Not_found -> bad "Not a known template name: %S" s
    in
    let destination_dir = ref None in
    let check_name name =
      if not (Str.string_match compilation_unit_name_regexp name 0) then
        bad "Not a valid compilation unit name: %s" name
    in
    let spec = Arg.(align [
      "-name", String (fun s -> check_name s; name := Some s),
      "<name> Name of the project (a valid compilation unit name)";
      "-template", String select_template,
      "basic The template for the project";
      "-target-directory", String (fun s -> destination_dir := Some s),
      "<dir> Generate the project in directory <dir> (the project's name by default)";
    ]) in
    Arg.(parse spec (bad "Don't know what to do with %S") usage_msg);
    match !template, !name with
      | template, Some name ->
        let dir = match !destination_dir with Some dir -> dir | None -> name in
        template, name, dir
      | _ -> Arg.usage spec usage_msg; exit 1
  in
  let env, preds, source_dir = init_project template name in
  create_project ~name ~env ~preds ~source_dir ~destination_dir:destination_dir

let () = main ()
