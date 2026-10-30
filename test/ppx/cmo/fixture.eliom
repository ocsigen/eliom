(* Values of many types sent from the server to the client. The client pass
   reads their types from the server cmo; the client output must compile. *)

type%shared t = A | B of int

let%server i = 1
let%server l = [1; 2]
let%server p = 1, "a"
let%server o = Some 1.
let%server v = B 1
let%server pv : [`A of int | `B] = `A 1

(* Injections. *)
let%client i = ~%i + 1
let%client l = List.length ~%l
let%client p = fst ~%p
let%client o = ~%o
let%client v = ~%v
let%client pv = ~%pv

(* Client values, and values sent with them. The server does not type client
   code: the types of client values come from their annotations. *)
let%server labelled () =
  [%client (fun ~x ?(y = 0) () -> x + y + ~%i : x:int -> ?y:int -> unit -> int)]

let%server obj () = [%client (object method m = List.length ~%l end : < m : int >)]
let%server shared_type () = [%client (~%v : t)]

let%server html () =
  [%client (Eliom.Content.Html.F.txt "a" : Html_types.txt Eliom.Content.Html.elt)]

let%shared from_shared (x : int) = [%client (~%x + 1 : int)]
