module Wrap = Eliom.Wrap

(* A wrapper is the last field of a block with tag 0. *)
type box = {content : string; boxw : box Wrap.wrapper}

let boxed content f = {content; boxw = Wrap.create_wrapper f}
let plain content = {content; boxw = Wrap.empty_wrapper}
let upper b = plain (String.uppercase_ascii b.content)
let contents l = List.map (fun b -> b.content) l

(* [unwrap v] is what a client gets from [v]: the wrapped value,
   marshalled then unmarshalled. The type of the result depends on the
   wrappers inside [v], hence the annotations at each use. Marshalling
   fails if closures remain, e.g. wrappers that were not applied. *)
let unwrap v =
  snd (Marshal.from_string (Marshal.to_string (Wrap.wrap v) []) 0 : _ * _)

let test_structure () =
  let v = [boxed "a" upper; plain "b"; boxed "c" upper] in
  let w : box list = unwrap v in
  Alcotest.(check (list string)) "wrapped" ["A"; "b"; "C"] (contents w);
  Alcotest.(check bool)
    "no wrapper left" true
    (List.for_all (fun b -> b.boxw = Wrap.empty_wrapper) w);
  Alcotest.(check (list string))
    "original unchanged" ["a"; "b"; "c"] (contents v)

let test_sharing () =
  let calls = ref 0 in
  let b = boxed "a" (fun b -> incr calls; upper b) in
  let w : box * box list = unwrap (b, [b; b]) in
  Alcotest.(check int) "wrapped once" 1 !calls;
  Alcotest.(check bool) "sharing kept" true (fst w == List.hd (snd w));
  Alcotest.(check string) "value" "A" (fst w).content

let test_nested () =
  (* The replacement is itself wrapped. *)
  let outer =
    boxed "outer" (fun _ -> [boxed "inner" upper; boxed "second" upper])
  in
  Alcotest.(check (list string))
    "nested" ["INNER"; "SECOND"]
    (contents (unwrap outer : box list))

type cycle = {cell : string; next : cycle option}

let test_cycle () =
  let rec c = {cell = "loop"; next = Some c} in
  let w : cycle * box = unwrap (c, boxed "a" upper) in
  Alcotest.(check string) "marked part" "A" (snd w).content;
  Alcotest.(check string) "cycle contents" "loop" (fst w).cell;
  match (fst w).next with
  | Some c' -> Alcotest.(check bool) "cycle kept" true (c' == fst w)
  | None -> Alcotest.fail "cycle lost"

let test_many () =
  (* Enough blocks to resize the tables of the traversal. *)
  let n = 20_000 in
  let v = List.init n (fun i -> string_of_int i, boxed "x" upper) in
  let w : (string * box) list = unwrap v in
  Alcotest.(check bool)
    "all wrapped" true
    (List.for_all2 (fun (i, b) (i', _) -> i = i' && b.content = "X") w v)

let test_minor_gc () =
  (* Many collections during the traversal: the tables are rehashed and
     resized while the blocks already visited move, and every value must
     still be found. *)
  let calls = ref 0 in
  let f b = incr calls; Gc.minor (); upper b in
  let n = 500 in
  let v = List.init n (fun i -> string_of_int i, boxed "x" f) in
  let w : (string * box) list = unwrap v in
  Alcotest.(check int) "wrapped once each" n !calls;
  Alcotest.(check bool)
    "all wrapped" true
    (List.for_all2 (fun (i, b) (i', _) -> i = i' && b.content = "X") w v);
  (* A young shared value, moved by a collection between its two visits,
     gets a second index. The number of blocks between the visits varies,
     so that the second visit sometimes triggers a resizing of the table
     and sometimes only a rehash: both merge the two indices, with what
     was computed for the first one. *)
  for k = 0 to 150 do
    let calls = ref 0 in
    let b = boxed "shared" (fun b -> incr calls; Gc.minor (); upper b) in
    let w : box * int list * box = unwrap (b, List.init k Fun.id, b) in
    let first, _, last = w in
    Alcotest.(check int)
      (Printf.sprintf "shared value wrapped once (%d blocks between)" k)
      1 !calls;
    Alcotest.(check bool)
      (Printf.sprintf "sharing kept (%d blocks between)" k)
      true (first == last)
  done

let test_exception () =
  (* A wrapper that raises: the exception is propagated and the GC
     settings changed during the traversal are restored. (The traversal
     changes max_overhead, which Gc.set ignores on OCaml 5, where the
     second check is trivially true.) *)
  let control = Gc.get () in
  let v = boxed "a" (fun _ -> failwith "wrapper") in
  Alcotest.check_raises "propagated" (Failure "wrapper") (fun () ->
    ignore (Wrap.wrap v));
  Alcotest.(check bool) "GC settings restored" true (Gc.get () = control)

let test_eliom_data () =
  let d = Eliom.Types.encode_eliom_data [boxed "a" upper; plain "b"] in
  let w : box list =
    snd (Marshal.from_string (Eliom.Lib.Url.decode d) 0 : _ * _)
  in
  Alcotest.(check (list string)) "decoded" ["A"; "b"] (contents w)

(* Escaping of marshalled data for single-quoted JavaScript strings in
   scripts of pages *)

(* [js_unescape s] is the value of the JavaScript literal ['s']. A digit
   after [\0] would make a legacy octal escape, forbidden in strict mode:
   it is an error. *)
let js_unescape s =
  let b = Buffer.create (String.length s) in
  let rec loop i =
    if i < String.length s
    then
      if s.[i] <> '\\'
      then (
        Buffer.add_char b s.[i];
        loop (i + 1))
      else
        match s.[i + 1] with
        | '0' ->
            if i + 2 < String.length s && s.[i + 2] >= '0' && s.[i + 2] <= '9'
            then Alcotest.failf "octal escape at %d in %S" i s;
            Buffer.add_char b '\000';
            loop (i + 2)
        | 'b' ->
            Buffer.add_char b '\b';
            loop (i + 2)
        | 't' ->
            Buffer.add_char b '\t';
            loop (i + 2)
        | 'n' ->
            Buffer.add_char b '\n';
            loop (i + 2)
        | 'f' ->
            Buffer.add_char b '\012';
            loop (i + 2)
        | 'r' ->
            Buffer.add_char b '\r';
            loop (i + 2)
        | 'x' ->
            Buffer.add_char b
              (Char.chr (int_of_string ("0x" ^ String.sub s (i + 2) 2)));
            loop (i + 4)
        | c ->
            Buffer.add_char b c;
            loop (i + 2)
  in
  loop 0; Buffer.contents b

let all_chars = String.init 256 Char.chr

let test_string_escape () =
  let check msg s =
    let e = Eliom.Lib.string_escape s in
    (* Outside escape sequences, no character may end the string or the
       script, or be mangled by the page encoding. *)
    let rec scan i =
      if i < String.length e
      then
        match e.[i] with
        | '\\' -> scan (i + if e.[i + 1] = 'x' then 4 else 2)
        | ('\'' | '<' | '>' | '&' | '\000' .. '\031' | '\127' .. '\255') as c ->
            Alcotest.failf "%s: %C in clear at %d in %S" msg c i e
        | _ -> scan (i + 1)
    in
    scan 0;
    Alcotest.(check string) msg s (js_unescape e)
  in
  check "all characters" all_chars;
  check "end of script" "</script><!--]]>";
  check "quotes" "'\"\\'";
  (* \0 followed by a digit would be an octal escape. *)
  check "null before a digit" "\0001\000";
  check "empty" "";
  check "marshalled value"
    (Marshal.to_string (Wrap.wrap ([boxed "a" upper], "</script>")) [])

let suite =
  ( "wrap"
  , [ Alcotest.test_case "structure" `Quick test_structure
    ; Alcotest.test_case "sharing" `Quick test_sharing
    ; Alcotest.test_case "nested" `Quick test_nested
    ; Alcotest.test_case "cycle" `Quick test_cycle
    ; Alcotest.test_case "many values" `Quick test_many
    ; Alcotest.test_case "minor collections" `Quick test_minor_gc
    ; Alcotest.test_case "wrapper exception" `Quick test_exception
    ; Alcotest.test_case "eliom data" `Quick test_eliom_data
    ; Alcotest.test_case "string escape" `Quick test_string_escape ] )
