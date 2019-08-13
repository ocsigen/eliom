open Migrate_parsetree

let migration =
  Versions.migrate Versions.ocaml_408 Versions.ocaml_current

let () =
  Compiler_libs.Ast_mapper.run_main
    (fun args -> migration.copy_mapper (Ppx_eliom_client.mapper args))
