type mime_type = string
type extension = string

module MapString = Map.Make(String)

type mime_list = mime_type MapString.t


let default_mime = ref "application/octet-stream"
let default_mime_type () = !default_mime


let parse_mime_types filename =
  let rec read_and_split mimemap in_ch =
    try
      let line = input_line in_ch in
      let line_upto =
        try
          let upto = String.index line '#' in
          String.sub line 0 upto
        with Not_found -> line
      in
      let strlist =
        Netstring_pcre.split (Netstring_pcre.regexp "\\s+") line_upto
      in
      match  strlist with
      | [] | [_] -> (* No extension on this line *) read_and_split mimemap in_ch
      | mime :: extensions ->
          let mimemap =
            List.fold_left (fun mimemap ext ->
                              MapString.add ext mime mimemap) mimemap extensions
          in
          read_and_split mimemap in_ch
    with End_of_file -> mimemap
  in
  try
    let in_ch = open_in filename in
    let map =
      (try
         read_and_split MapString.empty in_ch
       with e -> close_in in_ch; raise e)
    in
    close_in in_ch;
    map
  with Sys_error _ -> MapString.empty


let find_mime_type ?(default=(!default_mime)) ~mime_list ~extension =
  try MapString.find extension mime_list
  with Not_found -> default


let find_mime_type_file ?(default=(!default_mime)) ~mime_list ~filename =
  try
    let pos = (String.rindex filename '.') in
    let extension =
      String.sub filename
        (pos+1)
        ((String.length filename) - pos - 1)
    in
    find_mime_type ~default ~mime_list ~extension
  with Not_found -> default


let default_mime_list () =
  let parsed = ref None in
  match !parsed with
    | None ->
        let file = Ocsigen_config.get_mimefile () in
        Ocsigen_messages.debug
          (fun () -> Printf.sprintf "Loading mime types in '%s'" file);
        let map = parse_mime_types file in
        parsed := Some map;
        map
    | Some map -> map

