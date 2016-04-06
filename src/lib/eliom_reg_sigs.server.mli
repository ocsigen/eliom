(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2016 Vasilis Papavasileiou
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

module type PARAM = sig

  type page
  type options
  type result
  type return
  type maybe_ext

  val rt : (return, maybe_ext) Eliom_service.rt

  val send :
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    page ->
    Ocsigen_http_frame.result Lwt.t

  val send_appl_content : Eliom_service.send_appl_content
  (** Whether the service is capable to send application content when
      required. This field is usually [Eliom_service.XNever]. This
      value is recorded inside each service just after
      registration.  *)

  val result_of_http_result : Ocsigen_http_frame.result -> result

end

module type PARAM_ALPHA_RETURN = sig

  type ('a, 'b) page
  type options
  type 'a result

  val send :
    ?options:options ->
    ?charset:string ->
    ?code: int ->
    ?content_type:string ->
    ?headers: Http_headers.t ->
    (_, _) page ->
    Ocsigen_http_frame.result Lwt.t

  (** See {!Eliom_reg_sigs.PARAM.send_appl_content}. *)
  val send_appl_content : Eliom_service.send_appl_content

  val result_of_http_result : Ocsigen_http_frame.result -> _ result

end

module type S = sig

  type page
  type options
  type return
  type result

  include "sigs/eliom_reg_simpl.mli"

end
