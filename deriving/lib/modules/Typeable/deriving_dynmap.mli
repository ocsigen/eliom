(* Copyright Jeremy Yallop 2007.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Finite map : dynamic |-> t *)

open Deriving_Typeable

module Comp (T : Typeable) (E : Deriving_Eq.Eq with type a = T.a) :
sig
  type a = T.a
  val eq : dynamic -> dynamic -> bool
end

module DynMap :
sig
  type comparator = dynamic -> dynamic -> bool
  type 'a t
   val empty : 'a t
   val add : dynamic -> 'a -> comparator -> 'a t -> 'a t
   val mem : dynamic -> 'a t -> bool
   val find : dynamic -> 'a t -> 'a option
   val iter : (dynamic -> 'a -> unit) -> 'a t -> unit
end
