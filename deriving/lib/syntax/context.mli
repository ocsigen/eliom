(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast

(* Environment for code generation *)
type context = {
    (* name of the functor wrapping recursive modules definitions *)
    wrapper_id: Type.name;
    (* functor's parameters *)
    params: Type.expr list;
    (* Inside/Outside the wrapper *)
    inside: bool;
    (* ??? *)
    suffix: (Type.name * Type.name * Type.expr) option;
    (* mapping from type application or type parameters to module name *)
    names: Type.name Type.ExprMap.t;
    tnames: Type.name Type.ExprMap.t;
    decls: Type.decl Type.NameMap.t;
    generate: context -> Type.decl -> Type.subst -> Ast.module_expr;
    (* generate_sig: context -> Type.decl -> Ast.module_type *)
    (* generate_sig_expr: context -> Type.expr -> Ast.module_type *)
}

(* Static context for code generator. *)

module type Loc = sig
  val _loc : Loc.t (* location of the type definition being derived *)
end

module type Deriver = sig
  val generate: context -> Type.decl -> Type.subst -> Ast.module_expr
  (* val generate_sig: context -> Type.decl -> Ast.module_type *)
  (* val generate_sig_expr: context -> Type.decl -> Ast.module_type *)
end

module type RawDeriverDepends = sig
  val classname: Type.name
  val runtimename: Type.name
  val generate_expr: context -> Type.expr -> Ast.module_expr
end

module type DeriverDepends = functor (L: Loc) -> RawDeriverDepends

module type DeriverParams = sig
  val classname: Type.name (* deriver's name *)
  val runtimename: Type.name (* runtime module's name *)
  val predefs: (Type.qname * Type.name) list
  val default_module: (Type.name * Type.name) option
  val allow_private: bool
  val depends: (module DeriverDepends) list
end

module type DeriverHelpers = sig

  include DeriverParams
  include Loc

  module Untranslate : Type.Untranslate

  val seq: Ast.expr -> Ast.expr -> Ast.expr
  val seq_list: Ast.expr list -> Ast.expr

  val record_pattern: ?prefix:string -> Type.field list -> Ast.patt
  val record_expr: (string * Ast.expr) list -> Ast.expr
  val record_expression: ?prefix:string -> Type.field list -> Ast.expr

  val expr_list: Ast.expr list -> Ast.expr
  val patt_list: Ast.patt list -> Ast.patt

  val tuple_expr: Ast.expr list -> Ast.expr
  val tuple: ?prefix:string -> int -> string list * Ast.patt * Ast.expr

  val cast_pattern:
      context -> ?param:string -> Type.expr -> Ast.patt * Ast.expr * Ast.expr

  val unpack_depends: context -> Ast.expr -> Type.expr -> Ast.expr

  (* For pa_deriving_Pickle only *)
  val instantiate_modargs_repr: context -> Type.repr -> Type.repr

  class virtual generator:
    object

      method rhs: context -> Type.decl -> Type.subst -> Ast.module_expr
      method expr: context -> Type.expr -> Ast.module_expr

      method mapply: context -> Ast.module_expr -> Type.expr list -> Ast.expr

      method constr: context -> Type.qname * Type.expr list -> Ast.module_expr
      method param: context -> Type.param -> Ast.module_expr

      method wrap: context -> Type.expr -> Ast.str_item list -> Ast.module_expr

      method call_poly_expr: context -> Type.poly_expr -> string -> Ast.expr
      method call_expr: context -> Type.expr -> string -> Ast.expr

      method virtual sum:
	  ?eq:Type.expr -> context ->
	    Type.decl -> Type.expr Type.NameMap.t ->
	      Type.summand list -> Ast.str_item list
      method virtual tuple: context -> Type.expr list -> Ast.str_item list
      method virtual variant:
          context -> Type.decl -> Type.expr Type.NameMap.t ->
	    Type.variant -> Ast.str_item list
      method virtual record:
	  ?eq:Type.expr -> context ->
	    Type.decl -> Type.expr Type.NameMap.t ->
	      Type.field list -> Ast.str_item list

      method virtual alpha: context -> Type.param -> Ast.str_item list

      method class_: context -> [ `NYI ] -> Ast.str_item list
      method function_: context -> Type.expr * Type.expr -> Ast.str_item list
      method label:
          context ->
	    [ `NonOptional | `Optional ] * Type.name * Type.expr * Type.expr ->
	      Ast.str_item list
      method object_: context -> [ `NYI ] -> Ast.str_item list

    end

      (* val default_generate_sig: context -> Type.decl -> Ast.module_type *)
      (* val default_generate_sig_expr: context -> Type.decl -> Ast.module_type *)

end

module type MakeDeriver = functor (B : DeriverHelpers) -> Deriver
type deriver = (module DeriverParams) * (module MakeDeriver)
