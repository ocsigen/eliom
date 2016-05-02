(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_mkreg
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

module Make

    (Pages : Eliom_registration_sigs.PARAM_CLIENT) =

struct

  type page = Pages.page
  type options = Pages.options
  type return = Eliom_service.non_ocaml

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    Eliom_service.set_client_fun ?app ~service
      (fun g p ->
         lwt page = f g p in
         Pages.send ?options page)

  let create
      ?app ?scope:_ ?options:_ ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ?https ?name ?csrf_safe ?csrf_scope
      ?csrf_secure ?max_use ?timeout ~meth ~id ?error_handler
      f =
    let service =
      Eliom_service.create_unsafe
        ?name ?csrf_safe
        ?csrf_scope:(csrf_scope :> Eliom_common.user_scope option)
        ?csrf_secure ?max_use ?timeout ?https ~meth ~id ()
    in
    register ?app ~service f;
    service

end
