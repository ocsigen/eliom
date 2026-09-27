(** Values as sent to the client in a page. *)

val receive : 'a -> 'b
(** [receive v] is [v] as a client receives it: wrapped for the client
    ({!Eliom.Wrap.wrap}), marshalled and unmarshalled. Its type depends on
    the wrappers inside [v]: it is not checked. To be called by a service
    handler of the test server, where wrapping may register services, as for
    a page. *)
