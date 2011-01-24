(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Interned strings *)

type t
val compare : t -> t -> int
val eq : t -> t -> bool
val intern : string -> t
val to_string : t -> string
val name : t -> string
