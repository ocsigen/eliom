(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
 * Copyright (C) 2007 Vincent Balat
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


(** This module defines the functor to use to creates modules
   generating functions to register services for your own types of pages.
   It is used for example in {!Eliom_registration}.
 *)

open Eliom_lib

open Ocsigen_extensions
open Eliom_state
open Eliom_parameter
open Eliom_service

(** {2 Creating modules to register services for one type of pages} *)
module type REG_PARAM = "sigs/eliom_reg_param.mli"

module MakeRegister(Pages: REG_PARAM) : sig

  include "sigs/eliom_reg_unsafe.mli"
    subst type page := Pages.page
    and type options := Pages.options
    and type return := Pages.return
    and type result := Pages.result

end

(** {2 Creating modules to register services for one type of parametrised pages} *)

module type REG_PARAM_ALPHA_RETURN =
sig
  type ('a, 'b) page
  type 'a return
  type 'a result
  include "sigs/eliom_reg_param.mli"
    subst type page := ('a, 'b) page
      and type return := 'b return
      and type result := 'a result
end

module MakeRegister_AlphaReturn(Pages: REG_PARAM_ALPHA_RETURN) : sig

  include "sigs/eliom_reg_alpha_return_unsafe.mli"
     subst type page := ('a, 'b) Pages.page
       and type options := Pages.options
       and type return := 'b Pages.return
       and type result := 'a Pages.result

end

(**/**)
val suffix_redir_uri_key : string Polytables.key
