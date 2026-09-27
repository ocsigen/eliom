type t =
  {meth : [`GET | `POST]; action : string; fields : (string * string) list}

(* [find s sub i] is the position of the first [sub] in [s] from [i] *)
let find s sub i =
  let n = String.length s and m = String.length sub in
  let rec loop j =
    if j + m > n
    then None
    else if String.sub s j m = sub
    then Some j
    else loop (j + 1)
  in
  loop i

(* [after s sub i] is the position after the first [sub] in [s] from [i], or
   the end of [s] *)
let after s sub i =
  match find s sub i with
  | Some j -> j + String.length sub
  | None -> String.length s

let starts s i prefix =
  i + String.length prefix <= String.length s
  && String.sub s i (String.length prefix) = prefix

(* The entities of an attribute value or a text *)
let decode_entities s =
  let b = Buffer.create (String.length s) in
  let character entity =
    match entity with
    | "amp" -> Some (Uchar.of_char '&')
    | "lt" -> Some (Uchar.of_char '<')
    | "gt" -> Some (Uchar.of_char '>')
    | "quot" -> Some (Uchar.of_char '"')
    | "apos" -> Some (Uchar.of_char '\'')
    | e when String.length e > 1 && e.[0] = '#' ->
        let code =
          if e.[1] = 'x' || e.[1] = 'X'
          then int_of_string_opt ("0x" ^ String.sub e 2 (String.length e - 2))
          else int_of_string_opt (String.sub e 1 (String.length e - 1))
        in
        Option.bind code (fun c ->
          if Uchar.is_valid c then Some (Uchar.of_int c) else None)
    | _ -> None
  in
  let rec loop i =
    if i < String.length s
    then
      match s.[i], String.index_from_opt s i ';' with
      | '&', Some j -> (
        match character (String.sub s (i + 1) (j - i - 1)) with
        | Some u ->
            Buffer.add_utf_8_uchar b u;
            loop (j + 1)
        | None ->
            Buffer.add_char b '&';
            loop (i + 1))
      | c, _ ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0; Buffer.contents b

type tag = {name : string; closing : bool; attrs : (string * string) list}

let is_space = function ' ' | '\n' | '\t' | '\r' -> true | _ -> false

let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | ':' -> true
  | _ -> false

(* [tag s i] is the tag of [s] at [i], where [s.[i]] is ['<'], and the
   position after it *)
let tag s i =
  let n = String.length s in
  let rec skip p j = if j < n && p s.[j] then skip p (j + 1) else j in
  let closing = i + 1 < n && s.[i + 1] = '/' in
  let start = if closing then i + 2 else i + 1 in
  let stop = skip is_name_char start in
  let rec attrs j acc =
    let j = skip is_space j in
    if j >= n
    then List.rev acc, n
    else
      match s.[j] with
      | '>' -> List.rev acc, j + 1
      | '/' -> attrs (j + 1) acc
      | _ ->
          let k = skip is_name_char j in
          if k = j
          then attrs (j + 1) acc
          else
            let name = String.lowercase_ascii (String.sub s j (k - j)) in
            let k = skip is_space k in
            if k < n && s.[k] = '='
            then
              let v = skip is_space (k + 1) in
              let value, next =
                if v < n && (s.[v] = '"' || s.[v] = '\'')
                then
                  let e =
                    match String.index_from_opt s (v + 1) s.[v] with
                    | Some e -> e
                    | None -> failwith "Html_form: unterminated attribute"
                  in
                  String.sub s (v + 1) (e - v - 1), e + 1
                else
                  let e = skip (fun c -> not (is_space c || c = '>')) v in
                  String.sub s v (e - v), e
              in
              attrs next ((name, decode_entities value) :: acc)
            else attrs k ((name, name) :: acc)
  in
  let attrs, next = attrs stop [] in
  ( { name = String.lowercase_ascii (String.sub s start (stop - start))
    ; closing
    ; attrs }
  , next )

let attr name t = List.assoc_opt name t.attrs
let has name t = List.mem_assoc name t.attrs

(* The values of a select, from its options (value and selection) *)
let selected ~multiple options =
  match List.filter_map (fun (v, s) -> if s then Some v else None) options with
  | [] when multiple -> []
  | [] -> ( match options with (v, _) :: _ -> [v] | [] -> [])
  | l when multiple -> l
  | l -> [List.nth l (List.length l - 1)]

(* The fields of an input *)
let input t fields =
  match attr "name" t with
  | None -> fields
  | Some name -> (
    match Option.map String.lowercase_ascii (attr "type" t) with
    | Some ("checkbox" | "radio") ->
        if has "checked" t
        then (name, Option.value ~default:"on" (attr "value" t)) :: fields
        else fields
    | Some ("submit" | "button" | "image" | "reset" | "file") -> fields
    | _ -> (name, Option.value ~default:"" (attr "value" t)) :: fields)

let forms ~url page =
  let base = Uri.of_string ("http://host" ^ url) in
  let resolve action =
    Uri.path_and_query (Uri.resolve "http" base (Uri.of_string action))
  in
  (* [form] is the form being read, with its fields in reverse order, and
     [select] the select being read, with its options in reverse order *)
  let rec loop i form select forms =
    match String.index_from_opt page i '<' with
    | None -> List.rev forms
    | Some i when starts page i "<!--" ->
        loop (after page "-->" i) form select forms
    | Some i when starts page i "<!" || starts page i "<?" ->
        loop (after page ">" i) form select forms
    | Some i -> (
        let t, next = tag page i in
        let text_until close =
          let stop =
            Option.value ~default:(String.length page) (find page close next)
          in
          decode_entities (String.sub page next (stop - next)), stop
        in
        match t.name, t.closing, form with
        | ("script" | "style"), false, _ ->
            loop (after page ("</" ^ t.name) next) form select forms
        | "form", false, _ ->
            let meth =
              match Option.map String.lowercase_ascii (attr "method" t) with
              | Some "post" -> `POST
              | _ -> `GET
            in
            let action =
              resolve (Option.value ~default:url (attr "action" t))
            in
            loop next (Some (meth, action, [])) None forms
        | "form", true, Some (meth, action, fields) ->
            loop next None None
              ({meth; action; fields = List.rev fields} :: forms)
        | _, _, None -> loop next form select forms
        | "input", false, Some (meth, action, fields) ->
            loop next (Some (meth, action, input t fields)) select forms
        | "textarea", false, Some (meth, action, fields) ->
            let text, stop = text_until "</textarea" in
            let fields =
              match attr "name" t with
              | Some name -> (name, text) :: fields
              | None -> fields
            in
            loop stop (Some (meth, action, fields)) select forms
        | "select", false, _ ->
            loop next form (Some (attr "name" t, has "multiple" t, [])) forms
        | "option", false, _ -> (
          match select with
          | Some (name, multiple, options) ->
              let text, stop = text_until "</option" in
              let value =
                Option.value ~default:(String.trim text) (attr "value" t)
              in
              loop stop form
                (Some (name, multiple, (value, has "selected" t) :: options))
                forms
          | None -> loop next form select forms)
        | "select", true, Some (meth, action, fields) ->
            let fields =
              match select with
              | Some (Some name, multiple, options) ->
                  List.fold_left
                    (fun fields v -> (name, v) :: fields)
                    fields
                    (selected ~multiple (List.rev options))
              | _ -> fields
            in
            loop next (Some (meth, action, fields)) None forms
        | _ -> loop next form select forms)
  in
  loop 0 None None []

let set name value form =
  let rec replace = function
    | [] -> [name, value]
    | (n, _) :: rest when n = name ->
        (name, value) :: List.filter (fun (n, _) -> n <> name) rest
    | field :: rest -> field :: replace rest
  in
  {form with fields = replace form.fields}

let submit b {meth; action; fields} =
  match meth with
  | `GET ->
      Browser.get b
        (Uri.path_and_query (Uri.with_query' (Uri.of_string action) fields))
  | `POST -> Browser.post b action fields
