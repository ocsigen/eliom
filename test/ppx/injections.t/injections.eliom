let%server x = 1
let%server y = 2

(* Server values injected in client code are sent with the module. The same
   value injected twice is sent once. *)
let%client from_client = ~%x + ~%y + ~%x

(* An expression can be injected. *)
let%client expr = ~%(x * 2)

(* A qualified identifier gives its name to the injection. *)
let%client qualified = ~%Stdlib.max_int

(* In shared code, the server side uses the value itself. *)
let%shared from_shared = ~%x + 1
