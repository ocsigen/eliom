(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast
open Pa_deriving_common

open Utils
open Type
open Context
open Base

module Params = struct
  let classname = "Bounded"
  let runtimename = "Deriving_Bounded"
  let predefs = [
    ["unit"], "unit";
    ["bool"], "bool";
    ["char"], "char";
    ["int"], "int";
    ["int32"], "int32";
    ["Int32";"t"], "int32";
    ["int64"], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    (* ["open_flag"], "open_flag"; *)
    (* ["fpclass"], "fpclass"; *)
  ]
  let default_module = None
  let allow_private = false
  let depends = []
end

module Make(H : DeriverHelpers) : Deriver = struct

  open H

  let wrap min max =
    [ <:str_item< let min_bound = $min$ >>; <:str_item< let max_bound = $max$ >> ]

  let instance = object (self)

    inherit generator

    method tuple ctxt ts =
    let minBounds, maxBounds =
      List.split (List.map
                    (fun t -> let e = self#expr ctxt t in
                       <:expr< let module M = $e$ in M.min_bound >>,
                       <:expr< let module M = $e$ in M.max_bound >>) ts) in
    wrap (tuple_expr minBounds) (tuple_expr maxBounds)

    method sum ?eq ctxt ((tname,_,_,_,_) as decl) _ summands =
    let names = ListLabels.map summands
        ~f:(function
              | (name,[]) -> <:expr< $uid:name$ >>
              | (name,_) ->
		  raise (Underivable ("Bounded cannot be derived for the type "
                                      ^ tname ^ " because the constructor "
                                      ^ name ^ " is not nullary"))) in
    wrap (List.hd names) (List.last names)

    method variant ctxt decl subst (_, tags) =
      let names = ListLabels.map tags
          ~f:(function
            | Tag (name, []) -> <:expr< `$uid:name$ >>
            | Tag (name, _) ->
		raise (Underivable ("Bounded cannot be derived because the tag "
                                    ^ name ^ " is not nullary"))
            | _ -> raise (Underivable ("Bounded cannot be derived for this "
                                       ^ "polymorphic variant type"))) in
      wrap (List.hd names) (List.last names)

    (* should perhaps implement this one *)
    method record ?eq _ (tname,_,_,_,_) _ _ =
      raise (Underivable ("Bounded cannot be derived for record types (i.e. "
                          ^ tname ^ ")" ))

    method alpha ctxt ty = wrap <:expr< 0 >> <:expr< 0 >>

  end

  let generate = instance#rhs

end

let _ = Base.register (module Params: DeriverParams) (module Make : MakeDeriver)
