(* Vincent Balat 2005 *)  

(** Module Persistant: persistant data *)
(** Data are kept in memory but all modifications are stored in the database *)
(** When launching the program, if the value exists in the database,
    it is loaded, otherwise it is initialised to the default value *)

(** Type of persistent data *)
type 'a t

val make_persistant : name:string -> default:'a -> 'a t
(** [make_persistant name default] creates a persistent value named [name] 
    from database or create it with the default value [default] if it
    does not exist. *)

val get : 'a t -> 'a
(** [get pv] gives the value of [pv] *)

val set : 'a t -> 'a -> unit
(** [set pv value] sets a persistent value [pv] to [value] *)
