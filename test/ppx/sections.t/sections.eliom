(* Toplevel code is server code by default. *)
let default = 0

[%%client
let in_client = 1]

[%%shared
let in_shared = 2]

[%%server
let in_server = 3]

let%client let_client = 4
let%shared let_shared = 5
let%server let_server = 6

[%%client.start]

let after_client_start = 7

[%%shared.start]

let after_shared_start = 8

[%%server.start]

let after_server_start = 9

[%%shared.start]

(* An included structure is flattened: its items keep the current section. *)
include struct
  let included = 10
end

module%client M = struct
  let in_module = 11
end
