open Printf

let kind : [ `Server | `Client ] ref = ref `Server

let usage () =
  Printf.eprintf "Usage: %s -server <files>\n" (Filename.basename Sys.argv.(0));
  Printf.eprintf "Usage: %s -client <files>\n" (Filename.basename Sys.argv.(0));
  exit 1

let debug = ref false

let process_kind () =
  if Array.length Sys.argv < 2 then usage ();
  match Sys.argv.(1) with
  | "-server" -> `Server
  | "-client" -> `Client
  | _ -> usage ()

let print_debug lexbuf =
  let print_token = printf "%%%s:%s%%\n" in
  let print_token_c = printf "%%%s:%c%%\n" in
  match Eliompp_lexer.token lexbuf with
  | Eliompp_lexer.RAW s -> print_token "RAW" s
  | Eliompp_lexer.CHAR c -> print_token_c "CHAR" c
  | Eliompp_lexer.CLIENT_SECTION (loc, cnt) ->
      print_token ("CLIENT_SECTION" ^ string_of_int loc) cnt
  | Eliompp_lexer.SERVER_SECTION (loc, cnt) ->
      print_token ("SERVER_SECTION" ^ string_of_int loc) cnt
  | Eliompp_lexer.SHARED_SECTION (loc, cnt) ->
      print_token ("SHARED_SECTION" ^ string_of_int loc) cnt

let pretty_print_header filename =
  let print = printf "# %d \"%s\"\n" in
  print 1 filename;
  print 1 "<command-line>";
  print 1 filename

let current_section () = !Eliompp_lexer.section_idt
let we_are_ppx () = !Eliompp_lexer.we_are_ppx

let for_server () =
  match current_section () with `Server | `Shared -> true | `Client -> false

let for_client () =
  match current_section () with `Client | `Shared -> true | `Server -> false

let pretty_print filename lexbuf =
  (if !kind = `Client then
     match Eliompp_lexer.token lexbuf with
     | Eliompp_lexer.RAW s ->
         if we_are_ppx () && for_client () then printf "%s" s
     | Eliompp_lexer.CHAR c ->
         if we_are_ppx () && for_client () then printf "%c" c
     | Eliompp_lexer.CLIENT_SECTION (loc, cnt) ->
         printf "# %d \"%s\"\n%s\n\n" loc filename cnt
     | Eliompp_lexer.SHARED_SECTION (loc, cnt) ->
         printf "# %d \"%s\"\n%s\n\n" loc filename cnt
     | _ -> ()
   else
     match Eliompp_lexer.token lexbuf with
     | Eliompp_lexer.RAW s ->
         if (not (we_are_ppx ())) || for_server () then printf "%s" s
     | Eliompp_lexer.CHAR c ->
         if (not (we_are_ppx ())) || for_server () then printf "%c" c
     | Eliompp_lexer.SERVER_SECTION (loc, cnt) ->
         printf "# %d \"%s\"\n%s\n" loc filename cnt
     | Eliompp_lexer.SHARED_SECTION (loc, cnt) ->
         printf "# %d \"%s\"\n%s\n" loc filename cnt
     | _ -> ());
  flush stdout

let preprocess filename =
  try
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    pretty_print_header filename;
    while true do
      if !debug then print_debug lexbuf else pretty_print filename lexbuf
    done
  with
  | End_of_file -> ()
  | Parsing.Parse_error -> Printf.eprintf "[%s]: parsing error\n" filename

let process () =
  kind := process_kind ();
  let i = ref 2 in
  while !i < Array.length Sys.argv do
    (match Sys.argv.(!i) with
    | "-debug" -> debug := true
    | _ as filename -> preprocess filename);
    incr i
  done

let () = process ()
