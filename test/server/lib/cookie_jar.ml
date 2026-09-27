type cookie =
  { name : string
  ; value : string
  ; path : string
  ; expires : float option
  ; secure : bool }

(* In the order of creation *)
type t = cookie list

let empty = []

(* Dates of cookies, as written by Ocsigen Server:
   "Thu, 01 Jan 1970 00:00:00 GMT". *)
let months =
  [ "Jan"
  ; "Feb"
  ; "Mar"
  ; "Apr"
  ; "May"
  ; "Jun"
  ; "Jul"
  ; "Aug"
  ; "Sep"
  ; "Oct"
  ; "Nov"
  ; "Dec" ]

(* [days_from_civil y m d] is the number of days from 1970-01-01 to [y-m-d]
   (proleptic Gregorian calendar, [m] from 1 to 12). *)
let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era = (if y >= 0 then y else y - 399) / 400 in
  let yoe = y - (era * 400) in
  let doy = (((153 * if m > 2 then m - 3 else m + 9) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let month_index mon =
  let rec index i = function
    | [] -> None
    | m :: l -> if m = mon then Some i else index (i + 1) l
  in
  index 1 months

let parse_date s =
  match
    Scanf.sscanf s " %_s %d %s %d %d:%d:%d GMT" (fun d mon y h mi sec ->
      Option.map
        (fun m ->
           float_of_int
             ((days_from_civil y m d * 86400) + (h * 3600) + (mi * 60) + sec))
        (month_index mon))
  with
  | date -> date
  | exception (Scanf.Scan_failure _ | Failure _ | End_of_file) -> None

let split_at c s =
  Option.map
    (fun i ->
       ( String.trim (String.sub s 0 i)
       , String.trim (String.sub s (i + 1) (String.length s - i - 1)) ))
    (String.index_opt s c)

let attribute a =
  match split_at '=' a with
  | Some (n, v) -> String.lowercase_ascii n, v
  | None -> String.lowercase_ascii (String.trim a), ""

(* RFC 6265, 5.2.2: digits, with an optional minus sign *)
let max_age s =
  match int_of_string_opt s with
  | Some n when String.for_all (fun c -> c = '-' || (c >= '0' && c <= '9')) s ->
      Some n
  | _ -> None

(* RFC 6265, 5.1.4: the directory of the path of the request *)
let default_path path =
  match String.rindex_opt path '/' with
  | Some i when i > 0 && path.[0] = '/' -> String.sub path 0 i
  | _ -> "/"

(* RFC 6265, 5.2 and 5.3. The last occurrence of an attribute counts, and
   Max-Age takes precedence over Expires. *)
let parse ~now ~path header =
  let name_value, attributes =
    match String.index_opt header ';' with
    | Some i ->
        ( String.sub header 0 i
        , List.rev_map attribute
            (String.split_on_char ';'
               (String.sub header (i + 1) (String.length header - i - 1))) )
    | None -> header, []
  in
  match split_at '=' name_value with
  | None | Some ("", _) -> None
  | Some (name, value) ->
      let attribute n = List.assoc_opt n attributes in
      let path =
        match attribute "path" with
        | Some p when String.starts_with ~prefix:"/" p -> p
        | _ -> default_path path
      in
      let expires =
        match Option.bind (attribute "max-age") max_age with
        | Some s -> Some (now +. float_of_int s)
        | None -> Option.bind (attribute "expires") parse_date
      in
      Some
        {name; value; path; expires; secure = List.mem_assoc "secure" attributes}

let expired now c = match c.expires with Some e -> e <= now | None -> false
let same c c' = c.name = c'.name && c.path = c'.path

(* A cookie replacing another one keeps its place (RFC 6265, 5.3, 11.3), and
   an expired one removes it. *)
let add now jar c =
  let added = if expired now c then [] else [c] in
  if List.exists (same c) jar
  then List.concat_map (fun c' -> if same c c' then added else [c']) jar
  else jar @ added

let store ~now ~path set_cookies jar =
  List.fold_left
    (fun jar h ->
       match parse ~now ~path h with None -> jar | Some c -> add now jar c)
    jar set_cookies

(* RFC 6265, 5.1.4 *)
let path_matches ~cookie_path path =
  let n = String.length cookie_path in
  path = cookie_path
  || String.starts_with ~prefix:cookie_path path
     && (cookie_path.[n - 1] = '/' || path.[n] = '/')

let header ~now ~path jar =
  match
    List.filter
      (fun c ->
         (not c.secure)
         && (not (expired now c))
         && path_matches ~cookie_path:c.path path)
      jar
  with
  | [] -> None
  | l ->
      let l =
        List.stable_sort
          (fun c c' -> compare (String.length c'.path) (String.length c.path))
          l
      in
      Some (String.concat "; " (List.map (fun c -> c.name ^ "=" ^ c.value) l))

let cookies ~now jar =
  List.sort compare
    (List.filter_map
       (fun c -> if expired now c then None else Some (c.name, c.value))
       jar)
