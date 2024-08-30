#load "unix.cma"

#load "str.cma"

let app = Sys.argv.(1)

let modules_from_bytecode_executable nm =
  let ch = Unix.open_process_in (Printf.sprintf "ocamlobjinfo %s" nm) in
  while input_line ch <> "Imported units:" do
    ()
  done;
  let lst = ref [] in
  (try
     while
       let l = input_line ch in
       if l <> "" && l.[0] = '\t'
       then (
         let i = String.rindex l '\t' in
         lst := String.sub l (i + 1) (String.length l - i - 1) :: !lst;
         true)
       else false
     do
       ()
     done
   with End_of_file -> ());
  !lst

let modules_from_bytecode_library nm =
  let ch = Unix.open_process_in (Printf.sprintf "ocamlobjinfo %s" nm) in
  let lst = ref [] in
  (try
     while true do
       let l = input_line ch in
       if String.length l > 11 && String.sub l 0 11 = "Unit name: "
       then lst := String.sub l 11 (String.length l - 11) :: !lst
     done
   with End_of_file -> ());
  !lst

let read_file f =
  let ch = open_in f in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

let section_re = Str.regexp "close_\\(server\\|client\\)_section"

let match_substring sub_re s =
  try
    ignore (Str.search_forward sub_re s 0);
    true
  with Not_found -> false

let eliom_modules dir =
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.filter_map @@ fun nm ->
     if Filename.check_suffix nm ".pp.eliom"
     then
       let f = read_file (Filename.concat dir nm) in
       Some
         ( String.capitalize_ascii (Filename.chop_suffix nm ".pp.eliom")
         , match_substring section_re f )
     else None

let print_modules side l =
  Format.printf "[%%%%%s.start]@.@." side;
  List.iter (fun m -> Format.printf "module %s = %s@." m m) l;
  Format.printf "@."

let _ =
  let client_modules =
    modules_from_bytecode_executable (Printf.sprintf "./client/%s.bc" app)
  in
  let server_modules =
    modules_from_bytecode_library (Printf.sprintf "./%s.cma" app)
  in
  let eliom_modules = eliom_modules "." in
  let missing_server_modules =
    List.filter_map
      (fun (m, sect) ->
         let c = List.mem m client_modules in
         let s = List.mem m server_modules in
         match c, s, sect with true, false, true -> Some m | _ -> None)
      eliom_modules
  in
  let missing_client_modules =
    List.filter_map
      (fun (m, sect) ->
         let c = List.mem m client_modules in
         let s = List.mem m server_modules in
         match c, s, sect with false, true, true -> Some m | _ -> None)
      eliom_modules
  in
  let missing_modules =
    missing_server_modules <> [] || missing_client_modules <> []
  in
  if missing_modules
  then Format.eprintf "Some modules are missing in %s.eliom:@.@." app;
  if missing_server_modules <> []
  then print_modules "server" missing_server_modules;
  if missing_client_modules <> []
  then print_modules "client" missing_client_modules;
  if missing_modules then exit 1
