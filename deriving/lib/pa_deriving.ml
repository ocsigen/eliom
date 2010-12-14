(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Extend the OCaml grammar to include the `deriving' clause after
   type declarations in structure and signatures. *)

open Pa_deriving_common
open Utils

module Deriving (Syntax : Camlp4.Sig.Camlp4Syntax) =
struct

  open Camlp4.PreCast
  include Syntax

  DELETE_RULE Gram str_item: "type"; type_declaration END
  DELETE_RULE Gram sig_item: "type"; type_declaration END

  open Ast

  EXTEND Gram
  str_item:
  [[ "type"; types = type_declaration -> <:str_item< type $types$ >>
    | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP ","; ")" ->
        let decls = Base.display_errors _loc Type.Translate.decls types in
        let module U = Type.Untranslate(struct let _loc = _loc end) in
        let tdecls = List.map U.decl decls in
	let m = List.map (fun c -> Base.derive_str _loc decls (Base.find c)) cl in
        <:str_item< type $list:tdecls$ $list:m$ >>
   ]]
  ;
  sig_item:
  [[ "type"; types = type_declaration -> <:sig_item< type $types$ >>
   | "type"; types = type_declaration; "deriving"; "("; cl = LIST0 [x = UIDENT -> x] SEP "," ; ")" ->
       let decls  = Base.display_errors _loc Type.Translate.decls types in
       let module U = Type.Untranslate(struct let _loc = _loc end) in
       let tdecls = List.concat_map U.sigdecl decls in
       let ms = List.map (fun c -> Base.derive_sig _loc decls (Base.find c)) cl in
       <:sig_item< type $list:tdecls$ $list:ms$ >> ]]
  ;
  END

end

module M = Camlp4.Register.OCamlSyntaxExtension(Id)(Deriving)
