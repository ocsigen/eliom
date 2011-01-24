(* Ocsigen
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
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
(** XHTML "compact printing" (no pretty printing, no line breaks added) *)


module MakeCompact (Format : sig
  type 'a elt
  type doctypes
  val emptytags : string list
  val need_name : string list
  val doctype : doctypes -> string
  val default_doctype : doctypes
  val toeltl : 'a elt list -> XML.elt list
  val toelt : 'a elt -> XML.elt end) : sig
 
(** Ocsigen's compact printer for xhtml. [html_compat] is an option to set
    if you want to print with a syntax closer to html (not xml).
*)
  val xhtml_print :
    ?header:string ->
    ?version:Format.doctypes ->
    ?encode:(string -> string) ->
    ?html_compat:bool ->
    [ `Html ] Format.elt -> string

(** Ocsigen's compact printer for xhtml portions.
    [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml). *)
  val xhtml_list_print :
    ?header:string ->
    ?version: Format.doctypes ->
    ?encode:(string -> string) ->
    ?html_compat:bool ->
    'a Format.elt list -> string
    

(**/**)
  val emptytags : string list
end
