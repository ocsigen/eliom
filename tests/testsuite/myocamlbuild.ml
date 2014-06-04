open Ocamlbuild_plugin

module Conf = struct
  let server_dir = "server"
  let client_dir = "client"
  let type_dir = "type_dir"
end
module Eliom = Ocamlbuild_eliom.Make(Conf)

let more_dispatch = function
  | After_options ->
    pflag ["js_of_ocaml"] "jsopt" (fun n -> S [A n])
  | _ -> ()

let _ = dispatch (fun x ->
    more_dispatch x;
    Eliom.dispatcher x )
