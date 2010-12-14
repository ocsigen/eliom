(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

(* Registering type derivers into type-conv. *)

open Camlp4.PreCast

open Pa_deriving_common
open Context

let translate_str deriver types =
  let _loc = Ast.loc_of_ctyp types in
  let decls = Base.display_errors _loc Type.Translate.decls types in
  Base.derive_str _loc decls deriver

let translate_sig deriver types =
  let _loc = Ast.loc_of_ctyp types in
  let decls = Base.display_errors _loc Type.Translate.decls types in
  Base.derive_sig _loc decls deriver

let register (context, deriver) =
  let module Params = (val context : DeriverParams) in
  let name = String.uncapitalize Params.classname in
  Pa_type_conv.add_generator name (translate_str (context, deriver));
  Pa_type_conv.add_sig_generator name (translate_sig (context, deriver))

let _ = Base.add_register_hook register
