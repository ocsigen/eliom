(* Ocsigen
 * Copyright (C) Gabriel Kerneis
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

type attr = [ = `Attr of string | `CamlAttr of string ];
type valeur = [ = `Val of string | `CamlVal of string ];
type attribute = [ = `Attribute of (attr * valeur) | `CamlList of string ];
type token =
  [ Tag of (string * (list attribute) * bool)
  | PCData of string
  | Endtag of string
  | Comment of string
  | CamlString of string
  | CamlList of string
  | CamlExpr of string
  | Whitespace of string
  | Eof ];

