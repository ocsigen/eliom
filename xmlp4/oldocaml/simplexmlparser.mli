(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Basic camlp4 parser for xml *)

module ExprOrPatt :
  sig
    type tvarval = EPVstr of string | EPVvar of string
    type 'a tlist = PLEmpty | PLCons of 'a * 'a tlist
    type texprpatt =
        EPanyattr of tvarval * tvarval
      | EPanytag of string * texprpatt tlist * texprpatt tlist
      | EPpcdata of string
      | EPwhitespace of string
      | EPcomment of string
    (**/**)
    val loc : Lexing.position * Lexing.position
    val list_of_mlast_expr : MLast.expr list -> MLast.expr
    val list_of_mlast_patt : MLast.patt list -> MLast.patt
    val expr_valorval : tvarval -> MLast.expr
    val patt_valorval : tvarval -> MLast.patt
    val to_expr : texprpatt -> MLast.expr
    val to_expr_taglist : texprpatt tlist -> MLast.expr
    val to_expr_attlist : texprpatt tlist -> MLast.expr
    val to_patt : texprpatt -> MLast.patt
    val to_patt_taglist : texprpatt tlist -> MLast.patt
    val to_patt_attlist : texprpatt tlist -> MLast.patt
  end

(**/**)
val exprpatt_xml : ExprOrPatt.texprpatt Grammar.Entry.e
val exprpatt_any_tag : ExprOrPatt.texprpatt Grammar.Entry.e
val exprpatt_any_tag_list :
  ExprOrPatt.texprpatt ExprOrPatt.tlist Grammar.Entry.e
val exprpatt_any_attribute_list :
  ExprOrPatt.texprpatt ExprOrPatt.tlist Grammar.Entry.e
val exprpatt_attr_or_var : ExprOrPatt.tvarval Grammar.Entry.e
val exprpatt_value_or_var : ExprOrPatt.tvarval Grammar.Entry.e
val xml_exp : string -> MLast.expr
val xml_pat : string -> MLast.patt
val xmlparser : string -> ExprOrPatt.texprpatt ExprOrPatt.tlist
