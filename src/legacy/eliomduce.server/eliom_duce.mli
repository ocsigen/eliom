(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_duce
 * Copyright (C) 2007 Vincent Balat, Alain Frisch
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

(** Using OCamlduce for page generation. *)

open Eliom_duce_sigs

(** Register service and create form for Xhtml *)
module Xhtml : sig
  include XHTML_REGISTRATION
  include XHTML_FORMS
end

(** Register service for Xhtml *)
module Xhtml_registration : XHTML_REGISTRATION

(** Create form for Xhtml *)
module Xhtml_forms : XHTML_FORMS


(** Register and create form for any XML data type *)
(* module Xml : sig *)
  (* include XML_REGISTRATION *)
  (* include XHTML_FORMS *)
(* end *)

(** Register and create form for list of XML data type *)
(* module Xmllist : sig *)
  (* include XML_LIST_REGISTRATION *)
  (* include XHTML_FORMS *)
(* end *)

(** Register and create form for list of [blocks] (subtype of xhtml) *)
module Block : sig
  include BLOCKS_REGISTRATION
  include XHTML_FORMS
end

(* module SubXhtml : SUBXHTML *)
