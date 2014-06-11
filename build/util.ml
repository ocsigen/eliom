open Ocamlbuild_plugin
module Pack = Ocamlbuild_pack

let copy_with_header src prod =
  let contents = Pathname.read src in
  (* we need an empty line to keep the comments : weird camlp4 *)
  let header = "# 0 \"" ^ src ^ "\"\n\n" in
  Pack.Shell.mkdir_p (Filename.dirname prod);
  Echo ([header; contents], prod)

let copy_rule_with_header name src prod =
  rule name ~dep:src ~prod
    (fun env _ ->
       let prod = env prod in
       let src = env src in
       copy_with_header src prod
    )

let link source dest =
  rule (Printf.sprintf "%s -> %s" source dest) ~dep:source ~prod:dest
    (fun env _ -> Cmd (S [A"ln"; A"-f";P (env source); P (env dest)]))




let init () =
  (* add I pflag *)
  pflag [ "ocaml"; "compile"] "I" (fun x -> S[A"-I"; A x]);
  pflag [ "ocaml"; "infer_interface"] "I" (fun x -> S[A"-I"; A x]);
  pflag [ "ocaml"; "doc"] "I" (fun x -> S[A"-I"; A x]);
  flag ["ocaml"; "compile"; "thread"] (A "-thread");
  flag ["ocaml"; "link"; "thread"] (A "-thread");
  flag ["ocaml"; "infer_interface"] (A "-thread")
