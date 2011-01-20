(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Extending OCaml syntax with generic application. *)

open Camlp4.PreCast
open Utils
open Context

module Deriving (Syntax : Camlp4.Sig.Camlp4Syntax) =
struct
  open Camlp4.PreCast

  include Syntax

  EXTEND Gram
  expr: LEVEL "simple"
  [
  [ TRY [ e1 = val_longident ; "<" ; t = ctyp; ">"  ->
     match e1 with
       | <:ident< $uid:name$ . $lid:methodname$ >> ->
           if not (Base.is_registered name) then
             Base.fatal_error _loc ("deriving: "^ name ^" is not a known `class'");
	   let deriver = (Base.find name) in
	   let module Params = (val fst deriver : DeriverParams) in
	   <:expr<
	     let module $uid:Params.classname$ =
	       (val $Base.instantiate _loc t deriver$
		    : $uid:Params.runtimename$.$uid:Params.classname$
                           with type a = $t$) in
	     $uid:Params.classname$.$lid:methodname$ >>
       | <:ident< $uid:name$ >> ->
           if not (Base.is_registered name) then
             Base.fatal_error _loc ("deriving: "^ name ^" is not a known `class'");
	   Base.instantiate _loc t (Base.find name)
       | _ ->
           Base.fatal_error _loc
	     ("deriving: this looks a bit like a method application,"
              ^ "but the syntax is not valid");
  ]]];
  END

end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)

