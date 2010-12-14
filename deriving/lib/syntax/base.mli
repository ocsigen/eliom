(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(** module Pa_deriving_common.Base *)

open Camlp4.PreCast.Syntax

exception Underivable of string
exception NoSuchClass of string

(* display a fatal error and exit *)
val fatal_error : Loc.t -> string -> 'a
val display_errors : Loc.t -> ('a -> 'b) -> 'a -> 'b

open Context

module InContext(L:Loc)(D: DeriverParams) : DeriverHelpers

val register: (module DeriverParams) -> (module MakeDeriver) -> unit
val add_register_hook: (deriver -> unit) -> unit

val find: Type.name -> deriver
val is_registered: Type.name -> bool

val derive_str: Loc.t -> Type.decl list -> deriver -> Ast.str_item
val derive_sig: Loc.t -> Type.decl list -> deriver -> Ast.sig_item
