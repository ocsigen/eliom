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

let handle_file_client nm =
  let subdir_copy src dst =
    pf "(subdir Eliom\n (rule (copy# ../../%s %s)))\n" src dst
  in
  let copy_file extension =
    subdir_copy nm (Filename.chop_suffix nm extension ^ Filename.extension nm)
  in
  if Filename.check_suffix nm ".client.ml"
  then copy_file ".client.ml"
  else if Filename.check_suffix nm ".shared.ml"
  then copy_file ".shared.ml"
  else if Filename.check_suffix nm ".client.mli"
  then (
    copy_file ".client.mli";
    let modname = module_name nm in
    if Hashtbl.mem mli_only_client modname then subdir_copy nm (modname ^ ".ml"))
  else if Filename.check_suffix nm ".shared.mli"
  then (
    copy_file ".shared.mli";
    let modname = module_name nm in
    if Hashtbl.mem mli_only_client modname then subdir_copy nm (modname ^ ".ml"))
  else if Filename.check_suffix nm ".eliom"
  then
    let nm = Filename.chop_suffix nm ".eliom" in
    pf
      "(subdir Eliom\n\ (rule (target %s.ml)\n\  (deps ../../%s.eliom (file ../../server/.eliom_server.objs/byte/eliom__%s.cmo))\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_client --as-pp -internal -server-cmo server/.eliom_server.objs/byte/eliom__%s.cmo --impl %s.eliom))))))\n"
      nm nm
      (String.capitalize_ascii nm)
      (String.capitalize_ascii nm)
      nm
  else if Filename.check_suffix nm ".eliomi"
  then
    let nm = Filename.chop_suffix nm ".eliomi" in
    pf
      "(subdir Eliom\n\ (rule (target %s.mli) (deps ../../%s.eliomi)\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_client --as-pp -internal --intf %s.eliomi))))))\n"
      nm nm nm

let handle_file_server nm =
  let subdir_copy src dst =
    pf "(subdir Eliom\n (rule (copy# ../../%s %s)))\n" src dst
  in
  let copy_file extension =
    subdir_copy nm (Filename.chop_suffix nm extension ^ Filename.extension nm)
  in
  if Filename.check_suffix nm ".server.ml"
  then copy_file ".server.ml"
  else if Filename.check_suffix nm ".shared.ml"
  then copy_file ".shared.ml"
  else if Filename.check_suffix nm ".server.mli"
  then (
    copy_file ".server.mli";
    let modname = module_name nm in
    if Hashtbl.mem mli_only_server modname then subdir_copy nm (modname ^ ".ml"))
  else if Filename.check_suffix nm ".shared.mli"
  then (
    copy_file ".shared.mli";
    let modname = module_name nm in
    if Hashtbl.mem mli_only_server modname then subdir_copy nm (modname ^ ".ml"))
  else if Filename.check_suffix nm ".eliom"
  then
    let nm = Filename.chop_suffix nm ".eliom" in
    pf
      "(subdir Eliom\n\ (rule (target %s.ml) (deps ../../%s.eliom)\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_server --as-pp -internal --impl %s.eliom))))))\n"
      nm nm nm
  else if Filename.check_suffix nm ".eliomi"
  then
    let nm = Filename.chop_suffix nm ".eliomi" in
    pf
      "(subdir Eliom\n\ (rule (target %s.mli) (deps ../../%s.eliomi)\n\  (action\n\    (with-stdout-to %%{target}\n\      (chdir ../.. (run ppx_eliom_server --as-pp -internal --intf %s.eliomi))))))\n"
      nm nm nm

let () =
  let dir = Sys.argv.(2) in
  scan_mli_only dir;
  Sys.readdir dir |> Array.to_list |> List.sort compare
  |> List.iter
       (match Sys.argv.(1) with
       | "--server" -> handle_file_server
       | "--client" -> handle_file_client
       | _ -> assert false)
