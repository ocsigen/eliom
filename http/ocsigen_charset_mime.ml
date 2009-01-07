module MapString = Map.Make(String)
type extension = string


(* Handling of charset *)

type charset = string

type charset_assoc = {
  charset_assoc : charset MapString.t;
  charset_default: charset;
}

let no_charset = ""
let empty_charset_assoc ?(default=no_charset) () = {
  charset_assoc = MapString.empty;
  charset_default = no_charset;
}

let default_charset ~charset_assoc = charset_assoc.charset_default

let update_charset_assoc ~charset_assoc ~extension ~charset =
  { charset_assoc with charset_assoc =
      MapString.add extension charset charset_assoc.charset_assoc }

let set_default_charset ~charset ~charset_assoc =
  { charset_assoc with charset_default = charset }


let find_charset ~charset_assoc ~extension =
  try MapString.find (String.lowercase extension) charset_assoc.charset_assoc
  with Not_found -> charset_assoc.charset_default

let find_charset_file ~charset_assoc ~filename =
  try find_charset ~charset_assoc ~extension:(Ocsigen_lib.extension filename)
  with Not_found -> charset_assoc.charset_default




(* Handling of content-type *)

type mime_type = string


type mime_assoc = {
  mime_assoc : mime_type MapString.t;
  mime_default: mime_type;
}

let default_mime_type = "application/octet-stream"


let parse_mime_types ~filename =
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
  { mime_assoc =
      (try
         let in_ch = open_in filename in
         let map =
           (try
              read_and_split MapString.empty in_ch
            with e -> close_in in_ch; raise e)
         in
         close_in in_ch;
         map
       with Sys_error _ -> MapString.empty
      );
    mime_default = default_mime_type;
  }


let default_mime_assoc () =
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


let find_mime_type ~mime_assoc ~extension =
  try MapString.find (String.lowercase extension) mime_assoc.mime_assoc
  with Not_found -> mime_assoc.mime_default


let find_mime_type_file ~mime_assoc ~filename =
  try find_mime_type ~mime_assoc ~extension:(Ocsigen_lib.extension filename)
  with Not_found -> mime_assoc.mime_default

let update_mime_assoc ~mime_assoc ~extension ~mime =
  { mime_assoc with mime_assoc =
      MapString.add extension mime mime_assoc.mime_assoc }

let set_default_mime ~mime_type ~mime_assoc =
  { mime_assoc with mime_default = mime_type }

