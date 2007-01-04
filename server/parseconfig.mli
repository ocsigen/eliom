(* Ocsigen
 * http://www.ocsigen.org
 * Module parseconfig.ml
 * Copyright (C) 2005 Vincent Balat, Nataliya Guts
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

(** Config file parsing *)

val parse_size : string -> int64 option
val parse_string :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  string

(**/**)
val parser_config :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist list
val parse_server :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  unit
val extract_info :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  (string * string) *
  ((string option * string option) option * int list * int list)
val parse_config :
  unit ->
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist list
