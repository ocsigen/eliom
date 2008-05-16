(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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

(** XHTML pretty printing *)

(** A text included in each page *)
val ocsigenadv : string

(** Ocsigen's pretty printer for xhtml. [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml).
 *)
val xhtml_print :
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) ->
        ?html_compat:bool ->
          [ `Html ] XHTML.M.elt -> string

(** Ocsigen's pretty printer for xhtml portions.
   [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml). *)
val xhtml_list_print :
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) ->
        ?html_compat:bool ->
          'a XHTML.M.elt list -> string

(** Ocsigen's pretty printer for xhtml. [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml).
 *)
val xhtml_stream :
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) ->
        ?html_compat:bool ->
          [ `Html ] XHTML.M.elt -> string Ocsigen_stream.t

(** Ocsigen's pretty printer for xhtml portions.
   [html_compat] is an option to set
   if you want to print with a syntax closer to html (not xml). *)
val xhtml_list_stream :
    ?version:[< `HTML_v03_02 | `HTML_v04_01 | `XHTML_01_00 | `XHTML_01_01 > `XHTML_01_01 ] ->
      ?width:int -> ?encode:(string -> string) ->
        ?html_compat:bool ->
          'a XHTML.M.elt list -> string Ocsigen_stream.t

