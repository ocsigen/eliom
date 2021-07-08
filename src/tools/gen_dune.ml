
let handle_file_client nm =
  let copy_file extension =
    Printf.printf
      "(rule (copy# ../%s %s))\n"
      nm (Filename.chop_suffix nm extension ^ Filename.extension nm)
  in
  if Filename.check_suffix nm ".client.ml" then
    copy_file ".client.ml"
  else if Filename.check_suffix nm ".shared.ml" then
    copy_file ".shared.ml"
  else if Filename.check_suffix nm ".client.mli" then
    copy_file ".client.mli"
  else if Filename.check_suffix nm ".shared.mli" then
    copy_file ".shared.mli"
  else if Filename.check_suffix nm ".eliom" then
    let nm = Filename.chop_suffix nm ".eliom" in
    Printf.printf
      "(rule (target %s.ml) (deps ../%s.eliom ../server/%s.type_mli)\n\
      \  (action\n\
      \    (with-stdout-to %%{target}\n\
       \      (chdir .. (run ppx_eliom_client -type server/%s.type_mli --as-pp --impl %s.eliom)))))\n"
      nm nm nm nm nm

(*
let handle_file_server nm =
  if Filename.check_suffix nm ".eliom" then
    let nm = Filename.chop_suffix nm ".eliom" in
    Printf.printf
      "(rule (target %s.type_mli) (deps %s.eliom)\n\
      \  (action\n\
      \    (progn\n\
      \      (ignore-stdout (echo %%{cmo:%s}))\n\
      \      (with-stdout-to %%{target}\n\
      \        (pipe-stdout\n\
      \          (run ocamlc %%{read-lines:includes} -I .pervasives.objs/byte -I .bs.eobjs/byte -i -ppx \"%%{exe:ppx/eliom_ppx_type.exe} -as-ppx --prefix Bs_ --suffix _i18n --default-module Bs_i18n\" %s -impl %%{deps})\n\
      \          (run sed -e \"s$/[1-9][0-9]*$$g\" -e \"s/_\\\\[\\\\([<>]\\\\)/[\\\\1/g\" -e \"s/'\\\\(_[a-z0-9_]*\\\\)/'eliom_inferred_type_\\\\1/g\"))))))\n"
      nm nm nm
      (if nm <> "bs_pervasives" then "-open Bs_pervasives" else "")
 *)
let handle_file_server nm =
  let copy_file extension =
    Printf.printf
      "(rule (copy# ../%s %s))\n"
      nm (Filename.chop_suffix nm extension ^ Filename.extension nm)
  in
  if Filename.check_suffix nm ".server.ml" then
    copy_file ".server.ml"
  else if Filename.check_suffix nm ".shared.ml" then
    copy_file ".shared.ml"
  else if Filename.check_suffix nm ".server.mli" then
    copy_file ".server.mli"
  else if Filename.check_suffix nm ".shared.mli" then
    copy_file ".shared.mli"
  else if Filename.check_suffix nm ".eliom" then begin
    let nm = Filename.chop_suffix nm ".eliom" in
    Printf.printf
      "(rule (target %s.ml) (deps ../%s.eliom)\n\
      \  (action\n\
      \    (with-stdout-to %%{target}\n\
       \      (chdir .. (run ppx_eliom_server --as-pp --impl %%{deps})))))\n"
      nm nm;
    Printf.printf
      "(rule (target %s.type_mli) (deps ../%s.eliom)\n\
      \  (action\n\
      \    (progn\n\
      \      (ignore-stdout (echo %%{cmo:%s}))\n\
      \      (with-stdout-to %%{target}\n\
      \        (pipe-stdout\n\
      \          (chdir .. (run ocamlfind ocamlc -package lwt_ppx %%{read-lines:includes} -I server/.eliom_server.objs/byte -i -ppx \"%%{bin:ppx_eliom_types} --as-ppx\" -impl %%{deps}))\n\
      \          (run sed -e \"s$/[1-9][0-9]*$$g\" -e \"s/_\\\\[\\\\([<>]\\\\)/[\\\\1/g\" -e \"s/'\\\\(_[a-z0-9_]*\\\\)/'eliom_inferred_type_\\\\1/g\"))))))\n"
      nm nm nm
  end else if Filename.check_suffix nm ".eliomi" then
    let nm = Filename.chop_suffix nm ".eliomi" in
    Printf.printf
      "(rule (target %s.mli) (deps ../%s.eliomi)\n\
      \  (action\n\
      \    (with-stdout-to %%{target}\n\
       \      (chdir .. (run ppx_eliom_server --as-pp --intf %%{deps})))))\n"
      nm nm

let () =
  Sys.readdir (Sys.argv.(2)) |> Array.to_list |> List.sort compare
  |> List.iter
       (match Sys.argv.(1) with
        | "--server" -> handle_file_server
        | "--client" -> handle_file_client
        | _ -> assert false)
