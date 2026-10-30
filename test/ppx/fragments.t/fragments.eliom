(* A client value created by server code. *)
let%server simple () = [%client 1 + 1]

(* Its type, given by an annotation. *)
let%server annotated () = [%client (print_endline "x" : unit)]

(* Values of the server sent with the client value. The same value used twice is
   sent once. *)
let%server escaped x y = [%client ~%x + ~%y + ~%x]

(* An expression, not only an identifier, can be sent. *)
let%server escaped_expr x = [%client ~%(x + 1) * 2]

(* A client value created by shared code: the client side calls the function
   directly. *)
let%shared from_shared x = [%client ~%x + 1]

(* Unsafe client values may be polymorphic. *)
let%server unsafe () = [%client.unsafe fun x -> x]

(* A shared value: a server value and a client value. *)
let%server shared_value x = [%shared ~%x + 1]

(* A toplevel binding that is not a function may create client values while the
   module is loaded. *)
let%server at_toplevel = [%client 3]
