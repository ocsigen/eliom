
open Printf

let usage_msg =
  "Welcome to the Eliom destillery!\n\
   \n\
   This program generates a very simple Eliom application which\n\
   may serve as a starting point for your project. (Later versions\n\
   of it may generate much more scaffolding!)\n\
   \n\
   It accepts the following options, the mandatory ones are\n\
   marked with (*):"

let rec yes_no : default:bool -> string -> bool =
  fun ~default msg ->
  printf "%s (%s/%s) "
    msg
    (if default then "YES" else "yes")
    (if default then "no" else "NO");
  match String.(trim (lowercase (read_line ()))) with
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
                     (sprintf "Cannot create directory %s, it's a file" dir)) )
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
                         (Printf.sprintf "Cannot match %%%%endif%%%% in line %i in file %s"
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
         "File %s already exists! Overwrite?" dst_name
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
    ( if ksprintf (yes_no ~default:true) "Destination directory %s doesn't exists. Create it?" destination_dir then
        mkdir_p destination_dir
      else
        exit 1 );
  if not (Sys.is_directory destination_dir) then
    ( eprintf "Destination directory %s is a file!" destination_dir;
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

(* Configuration of the basic destillery project *)
let basic_project name =
  env name,
  preds (),
  Filename.concat Config.datadir Config.destillery_basic

let main () =
  let typ, name, destination_dir =
    let typ = ref `Basic in
    let name = ref None in
    let destination_dir = ref "." in
    let spec = Arg.([
      "-name", String (fun s -> name := Some s),
      "Name of the project, a valid compilation unit name (*)";
      "-destination", String (fun s -> destination_dir := s),
      "Destination directory";
    ]) in
    Arg.parse spec (fun _ -> Arg.usage spec usage_msg) usage_msg;
    match !name with
      | Some name -> !typ, name, !destination_dir
      | None -> Arg.usage spec usage_msg; exit 1
  in
  let env, preds, source_dir =
    match typ with
      | `Basic -> basic_project name
  in
  create_project ~name ~env ~preds ~source_dir ~destination_dir:destination_dir

let () = main ()
