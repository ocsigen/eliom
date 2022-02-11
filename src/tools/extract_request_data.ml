(*
Extract __eliom_request_data from an HTML page

ocaml str.cma ./extract_request_data.ml < /tmp/index.html | ocaml ./parse_request_data.ml
*)

let read ch =
  let b = Buffer.create 1024 in
  let s = Bytes.create 1024 in
  let rec read () =
    let n = input ch s 0 1024 in
    if n > 0
    then (
      Buffer.add_subbytes b s 0 n;
      read ())
  in
  read (); Buffer.contents b

let h c =
  match c with
  | '0' .. '9' -> Char.code c - Char.code '0'
  | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
  | _ -> assert false

let rec parse b s i =
  match s.[i] with
  | '\'' -> Buffer.contents b
  | '\\' -> (
    match s.[i + 1] with
    | '\'' ->
        Buffer.add_char b '\'';
        parse b s (i + 2)
    | 'x' ->
        Buffer.add_char b (Char.chr ((16 * h s.[i + 2]) + h s.[i + 3]));
        parse b s (i + 4)
    | '0' ->
        Buffer.add_char b '\000';
        parse b s (i + 2)
    | 't' ->
        Buffer.add_char b '\t';
        parse b s (i + 2)
    | 'b' ->
        Buffer.add_char b '\b';
        parse b s (i + 2)
    | 'n' ->
        Buffer.add_char b '\n';
        parse b s (i + 2)
    | 'r' ->
        Buffer.add_char b '\r';
        parse b s (i + 2)
    | 'f' ->
        Buffer.add_char b (Char.chr 12);
        parse b s (i + 2)
    | '\\' ->
        Buffer.add_char b '\\';
        parse b s (i + 2)
    | c ->
        Format.eprintf "??? %d %c@." i c;
        exit 1)
  | c ->
      Buffer.add_char b c;
      parse b s (i + 1)

let _ =
  let s = read stdin in
  let prefix = "__eliom_request_data = '" in
  let re = Str.regexp_string prefix in
  let i = Str.search_forward re s 0 in
  let b = Buffer.create 1024 in
  let s = parse b s (i + String.length prefix) in
  output_string stdout s
