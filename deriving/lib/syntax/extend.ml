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
  [e1 = TRY val_longident ; "<" ; t = ctyp; ">" ->
     match e1 with
       | <:ident< $uid:classname$ . $lid:methodname$ >> ->
           if not (Base.is_registered classname) then
             Base.fatal_error _loc ("deriving: "^ classname ^" is not a known `class'");
	   let module Loc = struct let _loc = _loc end in
	   let module U = Type.Untranslate(Loc) in
	   let binding = Ast.TyDcl (_loc, "inline", [], t, []) in
	   let decls = Base.display_errors _loc Type.Translate.decls binding in
	   if List.exists Type.contains_tvars_decl decls then
	     Base.fatal_error _loc
	       ("deriving: type variables cannot be used in `method' instantiations");
	   let tdecls = List.map U.decl decls in
	   let deriver = (Base.find classname) in
	   let m = Base.derive_str _loc decls deriver in
	   let module Params = (val fst deriver : DeriverParams) in
	   <:expr< let module M = struct
	     type $list:tdecls$
		 $m$
	     include $uid:classname ^ "_inline"$
	   end in
	   let module $uid:classname$ =
	     (val M.make : $uid:Params.runtimename$.$uid:Params.classname$
                           with type a = M.inline) in
	   $uid:Params.classname$.$lid:methodname$ >>
       | _ ->
           Base.fatal_error _loc
	     ("deriving: this looks a bit like a method application,"
              ^ "but the syntax is not valid");
  ]];
  END

end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)

