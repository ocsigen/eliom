(* A persistent reference of global scope created at the toplevel of a
   module, as a library does, before the server program sets the location
   of the database. *)

let value =
  Eliom.Reference.eref ~scope:Eliom.Common.global_scope
    ~persistent:("test_global_ref", [%json: string])
    ""

(* Something slow at the toplevel of a module, here a sleep, lets the
   threads of the database run before the database file is set. The
   reference must not access the database before it is used. *)
let () = Unix.sleepf 0.2
