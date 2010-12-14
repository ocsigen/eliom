(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast

open Pa_deriving_common
open Context

module Params = struct
  let classname = "Dummy"
  let runtimename = "Deriving_Dummy"
  let predefs = [
    ["unit"     ], "unit";
    ["bool"     ], "bool";
    ["char"     ], "char";
    ["int"      ], "int";
    ["int32"    ], "int32";
    ["Int32";"t"], "int32";
    ["int64"    ], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"    ], "float";
    (* ["num"], "num"; *)
    ["string"   ], "string";
    ["list"     ], "list";
    ["ref"      ], "ref";
    ["option"   ], "option";
    ["array"    ], "array";
  ]
  let default_module = None
  let allow_private = true
  let depends = []
end

module Make(H : DeriverHelpers) : Deriver = struct

  (* include H *)
  let _loc = H._loc

  let instance = object (self)

    inherit H.generator

    method tuple ctxt args =
      [ <:str_item< let test () = print_endline "Dummy tuple" >> ]

    method sum ?eq ctxt decl subst summands =
      [ <:str_item< let test () = print_endline "Dummy sum" >> ]

    method record ?eq ctxt decl subst fields =
      [ <:str_item< let test () = print_endline "Dummy record" >> ]

    method variant ctxt decl subst (_,tags) =
      [ <:str_item< let test () = print_endline "Dummy variant" >> ]

    method alpha ctxt ty = [ <:str_item< let test () = assert false >> ]

  end

  let generate = instance#rhs
  (* let generate_sig = H.default_generate_sig *)

end

let _ =
  Base.register (module Params : DeriverParams) (module Make : MakeDeriver)
