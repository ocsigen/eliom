(* Eliom
 * http://www.ocsigen.org
 * Module eliom_mod.ml
 * Copyright (C) 2012 Grégoire Henry
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

open Eliom_pervasives
open HTML5

val appendChild: ?before:'a elt -> 'b elt -> 'c elt -> unit
val removeChild: 'a elt -> 'b elt -> unit
val replaceChild: 'a elt -> 'b elt -> 'c elt -> unit
val removeAllChild: 'a elt -> unit
val replaceAllChild: 'a elt -> 'b elt list -> unit

val addEventListener:
  ?capture:bool ->
  'a elt ->
  (#Dom_html.event as 'b) Js.t Dom_events.Typ.typ ->
  ('a elt -> 'b Js.t -> unit) ->
  Dom_events.listener

val scrollIntoView: ?bottom:bool -> 'a elt -> unit

val childNodes: 'a elt -> Dom.node Js.t list
val childElements: 'a elt -> Dom.element Js.t list

module Css : sig
  val background: 'a elt -> string
  val backgroundAttachment: 'a elt -> string
  val backgroundColor: 'a elt -> string
  val backgroundImage: 'a elt -> string
  val backgroundPosition: 'a elt -> string
  val backgroundRepeat: 'a elt -> string
  val border: 'a elt -> string
  val borderBottom: 'a elt -> string
  val borderBottomColor: 'a elt -> string
  val borderBottomStyle: 'a elt -> string
  val borderBottomWidth: 'a elt -> string
  val borderCollapse: 'a elt -> string
  val borderColor: 'a elt -> string
  val borderLeft: 'a elt -> string
  val borderLeftColor: 'a elt -> string
  val borderLeftStyle: 'a elt -> string
  val borderLeftWidth: 'a elt -> string
  val borderRight: 'a elt -> string
  val borderRightColor: 'a elt -> string
  val borderRightStyle: 'a elt -> string
  val borderRightWidth: 'a elt -> string
  val borderSpacing: 'a elt -> string
  val borderStyle: 'a elt -> string
  val borderTop: 'a elt -> string
  val borderTopColor: 'a elt -> string
  val borderTopStyle: 'a elt -> string
  val borderTopWidth: 'a elt -> string
  val borderWidth: 'a elt -> string
  val bottom: 'a elt -> string
  val captionSide: 'a elt -> string
  val clear: 'a elt -> string
  val clip: 'a elt -> string
  val color: 'a elt -> string
  val content: 'a elt -> string
  val counterIncrement: 'a elt -> string
  val counterReset: 'a elt -> string
  val cssFloat: 'a elt -> string
  val cssText: 'a elt -> string
  val cursor: 'a elt -> string
  val direction: 'a elt -> string
  val display: 'a elt -> string
  val emptyCells: 'a elt -> string
  val font: 'a elt -> string
  val fontFamily: 'a elt -> string
  val fontSize: 'a elt -> string
  val fontStyle: 'a elt -> string
  val fontVariant: 'a elt -> string
  val fontWeight: 'a elt -> string
  val height: 'a elt -> string
  val left: 'a elt -> string
  val letterSpacing: 'a elt -> string
  val lineHeight: 'a elt -> string
  val listStyle: 'a elt -> string
  val listStyleImage: 'a elt -> string
  val listStylePosition: 'a elt -> string
  val listStyleType: 'a elt -> string
  val margin: 'a elt -> string
  val marginBottom: 'a elt -> string
  val marginLeft: 'a elt -> string
  val marginRight: 'a elt -> string
  val marginTop: 'a elt -> string
  val maxHeight: 'a elt -> string
  val maxWidth: 'a elt -> string
  val minHeight: 'a elt -> string
  val minWidth: 'a elt -> string
  val opacity: 'a elt -> string option
  val outline: 'a elt -> string
  val outlineColor: 'a elt -> string
  val outlineOffset: 'a elt -> string
  val outlineStyle: 'a elt -> string
  val outlineWidth: 'a elt -> string
  val overflow: 'a elt -> string
  val overflowX: 'a elt -> string
  val overflowY: 'a elt -> string
  val padding: 'a elt -> string
  val paddingBottom: 'a elt -> string
  val paddingLeft: 'a elt -> string
  val paddingRight: 'a elt -> string
  val paddingTop: 'a elt -> string
  val pageBreakAfter: 'a elt -> string
  val pageBreakBefore: 'a elt -> string
  val position: 'a elt -> string
  val right: 'a elt -> string
  val tableLayout: 'a elt -> string
  val textAlign: 'a elt -> string
  val textDecoration: 'a elt -> string
  val textIndent: 'a elt -> string
  val textTransform: 'a elt -> string
  val top: 'a elt -> string
  val verticalAlign: 'a elt -> string
  val visibility: 'a elt -> string
  val whiteSpace: 'a elt -> string
  val width: 'a elt -> string
  val wordSpacing: 'a elt -> string
  val zIndex: 'a elt -> string
end

module SetCss : sig
  val background: 'a elt -> string -> unit
  val backgroundAttachment: 'a elt -> string -> unit
  val backgroundColor: 'a elt -> string -> unit
  val backgroundImage: 'a elt -> string -> unit
  val backgroundPosition: 'a elt -> string -> unit
  val backgroundRepeat: 'a elt -> string -> unit
  val border: 'a elt -> string -> unit
  val borderBottom: 'a elt -> string -> unit
  val borderBottomColor: 'a elt -> string -> unit
  val borderBottomStyle: 'a elt -> string -> unit
  val borderBottomWidth: 'a elt -> string -> unit
  val borderCollapse: 'a elt -> string -> unit
  val borderColor: 'a elt -> string -> unit
  val borderLeft: 'a elt -> string -> unit
  val borderLeftColor: 'a elt -> string -> unit
  val borderLeftStyle: 'a elt -> string -> unit
  val borderLeftWidth: 'a elt -> string -> unit
  val borderRight: 'a elt -> string -> unit
  val borderRightColor: 'a elt -> string -> unit
  val borderRightStyle: 'a elt -> string -> unit
  val borderRightWidth: 'a elt -> string -> unit
  val borderSpacing: 'a elt -> string -> unit
  val borderStyle: 'a elt -> string -> unit
  val borderTop: 'a elt -> string -> unit
  val borderTopColor: 'a elt -> string -> unit
  val borderTopStyle: 'a elt -> string -> unit
  val borderTopWidth: 'a elt -> string -> unit
  val borderWidth: 'a elt -> string -> unit
  val bottom: 'a elt -> string -> unit
  val captionSide: 'a elt -> string -> unit
  val clear: 'a elt -> string -> unit
  val clip: 'a elt -> string -> unit
  val color: 'a elt -> string -> unit
  val content: 'a elt -> string -> unit
  val counterIncrement: 'a elt -> string -> unit
  val counterReset: 'a elt -> string -> unit
  val cssFloat: 'a elt -> string -> unit
  val cssText: 'a elt -> string -> unit
  val cursor: 'a elt -> string -> unit
  val direction: 'a elt -> string -> unit
  val display: 'a elt -> string -> unit
  val emptyCells: 'a elt -> string -> unit
  val font: 'a elt -> string -> unit
  val fontFamily: 'a elt -> string -> unit
  val fontSize: 'a elt -> string -> unit
  val fontStyle: 'a elt -> string -> unit
  val fontVariant: 'a elt -> string -> unit
  val fontWeight: 'a elt -> string -> unit
  val height: 'a elt -> string -> unit
  val left: 'a elt -> string -> unit
  val letterSpacing: 'a elt -> string -> unit
  val lineHeight: 'a elt -> string -> unit
  val listStyle: 'a elt -> string -> unit
  val listStyleImage: 'a elt -> string -> unit
  val listStylePosition: 'a elt -> string -> unit
  val listStyleType: 'a elt -> string -> unit
  val margin: 'a elt -> string -> unit
  val marginBottom: 'a elt -> string -> unit
  val marginLeft: 'a elt -> string -> unit
  val marginRight: 'a elt -> string -> unit
  val marginTop: 'a elt -> string -> unit
  val maxHeight: 'a elt -> string -> unit
  val maxWidth: 'a elt -> string -> unit
  val minHeight: 'a elt -> string -> unit
  val minWidth: 'a elt -> string -> unit
  val opacity: 'a elt -> string option -> unit
  val outline: 'a elt -> string -> unit
  val outlineColor: 'a elt -> string -> unit
  val outlineOffset: 'a elt -> string -> unit
  val outlineStyle: 'a elt -> string -> unit
  val outlineWidth: 'a elt -> string -> unit
  val overflow: 'a elt -> string -> unit
  val overflowX: 'a elt -> string -> unit
  val overflowY: 'a elt -> string -> unit
  val padding: 'a elt -> string -> unit
  val paddingBottom: 'a elt -> string -> unit
  val paddingLeft: 'a elt -> string -> unit
  val paddingRight: 'a elt -> string -> unit
  val paddingTop: 'a elt -> string -> unit
  val pageBreakAfter: 'a elt -> string -> unit
  val pageBreakBefore: 'a elt -> string -> unit
  val position: 'a elt -> string -> unit
  val right: 'a elt -> string -> unit
  val tableLayout: 'a elt -> string -> unit
  val textAlign: 'a elt -> string -> unit
  val textDecoration: 'a elt -> string -> unit
  val textIndent: 'a elt -> string -> unit
  val textTransform: 'a elt -> string -> unit
  val top: 'a elt -> string -> unit
  val verticalAlign: 'a elt -> string -> unit
  val visibility: 'a elt -> string -> unit
  val whiteSpace: 'a elt -> string -> unit
  val width: 'a elt -> string -> unit
  val wordSpacing: 'a elt -> string -> unit
  val zIndex: 'a elt -> string -> unit
end
