let pf = Printf.printf

let module_name nm =
  try
    let nm = Filename.chop_extension nm in
    try Filename.chop_extension nm with Invalid_argument _ -> nm
  with Invalid_argument _ -> nm

let mli_only_server = Hashtbl.create 16
let mli_only_client = Hashtbl.create 16

let scan_mli_only dir =
  let files = Sys.readdir dir |> Array.to_list in
  let has_impl_for modname suffixes =
    List.exists
      (fun f ->
         module_name f = modname
         && List.exists (fun s -> Filename.check_suffix f s) suffixes)
      files
  in
  List.iter
    (fun f ->
       let modname = module_name f in
       if
         Filename.check_suffix f ".server.mli"
         || Filename.check_suffix f ".shared.mli"
       then
         if not (has_impl_for modname [".server.ml"; ".shared.ml"; ".eliom"])
         then Hashtbl.replace mli_only_server modname true;
       if
         Filename.check_suffix f ".client.mli"
         || Filename.check_suffix f ".shared.mli"
       then
         if not (has_impl_for modname [".client.ml"; ".shared.ml"; ".eliom"])
         then Hashtbl.replace mli_only_client modname true)
    files

let subdir_copy src dst =
  pf "(subdir Eliom\n (rule (copy# ../../%s %s)))\n" src dst

(* Copy [nm] to Eliom/, without the [extension] side suffix *)
let copy_file nm extension =
  subdir_copy nm (Filename.chop_suffix nm extension ^ Filename.extension nm)

(* Copy an interface to Eliom/, and also use it as implementation if the
   module has no implementation on this side *)
let copy_interface ~mli_only nm extension =
  copy_file nm extension;
  let modname = module_name nm in
  if Hashtbl.mem mli_only modname then subdir_copy nm (modname ^ ".ml")

let client_eliom_rule nm =
  pf
    "(subdir Eliom\n\ (rule (target %s.ml)\n\  (deps ../../%s.eliom (file ../../server/.eliom_server.objs/byte/eliom__%s.cmo))\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_client --as-pp -internal -server-cmo server/.eliom_server.objs/byte/eliom__%s.cmo --impl %s.eliom))))))\n"
    nm nm
    (String.capitalize_ascii nm)
    (String.capitalize_ascii nm)
    nm

let server_eliom_rule nm =
  pf
    "(subdir Eliom\n\ (rule (target %s.ml) (deps ../../%s.eliom)\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_server --as-pp -internal --impl %s.eliom))))))\n"
    nm nm nm

let eliomi_rule ~side nm =
  pf
    "(subdir Eliom\n\ (rule (target %s.mli) (deps ../../%s.eliomi)\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_%s --as-pp -internal --intf %s.eliomi))))))\n"
    nm nm side nm

(* Print the rules for file [nm] on the given side ("client" or "server") *)
let handle_file ~side ~mli_only ~eliom_rule nm =
  let side_ml = "." ^ side ^ ".ml" and side_mli = "." ^ side ^ ".mli" in
  let is extension = Filename.check_suffix nm extension in
  if is side_ml
  then copy_file nm side_ml
  else if is ".shared.ml"
  then copy_file nm ".shared.ml"
  else if is side_mli
  then copy_interface ~mli_only nm side_mli
  else if is ".shared.mli"
  then copy_interface ~mli_only nm ".shared.mli"
  else if is ".eliom"
  then eliom_rule (Filename.chop_suffix nm ".eliom")
  else if is ".eliomi"
  then eliomi_rule ~side (Filename.chop_suffix nm ".eliomi")

let handle_file_client =
  handle_file ~side:"client" ~mli_only:mli_only_client
    ~eliom_rule:client_eliom_rule

let handle_file_server =
  handle_file ~side:"server" ~mli_only:mli_only_server
    ~eliom_rule:server_eliom_rule

let () =
  let dir = Sys.argv.(2) in
  scan_mli_only dir;
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.iter
       (match Sys.argv.(1) with
       | "--server" -> handle_file_server
       | "--client" -> handle_file_client
       | _ -> assert false)
