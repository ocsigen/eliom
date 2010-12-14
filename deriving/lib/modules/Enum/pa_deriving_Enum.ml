(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast
open Pa_deriving_common

open Context
open Base
open Type
open Utils

module Params = struct
  let classname = "Enum"
  let runtimename = "Deriving_Enum"
  let predefs = [
    ["int"], "int";
    ["bool"], "bool";
    ["unit"], "unit";
    ["char"], "char";
  ]
  let default_module = Some ("Defaults", "min")
  let allow_private = false
  let depends = []
end

module Make(H : DeriverHelpers) : Deriver = struct

  open H

  let wrap numbering = [ <:str_item< let numbering = $numbering$ >> ]

  let instance = object(self)

    inherit generator

    method sum ?eq ctxt ((tname,_,_,_,_) as decl) subst summands =
      let numbering =
	List.fold_right2
          (fun n ctor rest ->
            match ctor with
            | (name, []) -> <:expr< ($uid:name$, $`int:n$) :: $rest$ >>
            | (name,_) ->
		raise (Underivable ("Enum cannot be derived for the type "
				    ^ tname ^ " because the constructor "
				    ^ name ^ " is not nullary" )))
          (List.range 0 (List.length summands))
          summands
          <:expr< [] >> in
      wrap numbering

    method variant ctxt decl subst (_, tags) =
      let numbering =
	List.fold_right2
          (fun n tagspec rest ->
            match tagspec with
            | Tag (name, []) -> <:expr< (`$name$, $`int:n$) :: $rest$ >>
            | Tag (name, _) ->
		raise (Underivable ("Enum cannot be derived because the tag "
                                    ^ name ^ " is not nullary" ))
            | _ -> raise (Underivable ("Enum cannot be derived for this "
                                       ^ "polymorphic variant type")))
          (List.range 0 (List.length tags))
          tags
          <:expr< [] >> in
      wrap numbering

    method tuple context _ =
      raise (Underivable "Enum cannot be derived for tuple types")

    method record ?eq _ (tname,_,_,_,_) _ _ =
      raise (Underivable ("Enum cannot be derived for record types (i.e. "^tname^")"))

    method alpha ctxt ty = wrap <:expr< [] >>

  end

  let generate = instance#rhs

end

let _ = Base.register (module Params: DeriverParams) (module Make : MakeDeriver)
