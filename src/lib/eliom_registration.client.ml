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

module type Base = sig
  type return = Eliom_service.non_ocaml
end

module Base = struct
  type return = Eliom_service.non_ocaml
end

module Block5 = Base
module Html_text = Base
module CssText = Base
module Text = Base
module String = Base

module Unit = Base

module String_redirection = Base

module Any = Base
module Streamlist = Base

module Ocaml = struct
  type 'a return = 'a Eliom_service.ocaml
end

module Html = struct

  type page = Html_types.html Eliom_content.Html.elt
  type options = unit
  type return = Eliom_service.non_ocaml

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    Eliom_service.set_client_fun ?app ~service @@ fun g p ->
    lwt page = f g p in
    Eliom_client.set_content_local
      (Eliom_content.Html.To_dom.of_element page)

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

module Action = struct

  type page    = unit
  type options = [`Reload | `NoReload]
  type return  = Eliom_service.non_ocaml

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    (* The action has no reload function of its own
       (reset_reload_fun). If it did, Eliom_client.change_page would
       get us into an infinite loop. We rely on the reload_function of
       the previously-called service. *)
    Eliom_service.set_client_fun ?app ~service
      (fun g p ->
         (* See explanation in Eliom_client *)
         Eliom_client.do_not_set_uri := true;
         lwt () = f g p in
         match !Eliom_client.reload_function, options with
         | Some rf, (Some `Reload | None) ->
           rf () ()
         | None, (Some `Reload | None) ->
           (* last page was probably generated on the server, so
              default a standard reload *)
           Dom_html.window##location##reload();
           Lwt.return ()
         | _, Some `NoReload ->
           Lwt.return ());
    Eliom_service.reset_reload_fun service

  let create
      ?app ?scope:_ ?options:_ ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ?https ?name ?csrf_safe ?csrf_scope
      ?csrf_secure ?max_use ?timeout ~meth ~id ?error_handler
      f =
    let service =
      Eliom_service.create
        ?name ?csrf_safe
        ?csrf_scope:(csrf_scope :> Eliom_common.user_scope option)
        ?csrf_secure ?max_use ?timeout ?https ~meth ~id ()
    in
    register ?app ~service f;
    service

end

module App (P : Eliom_registration_sigs.APP_PARAM) = struct
  let application_name = P.application_name
  include Html
end

type _ redirection =
    Redirection :
      (unit, unit, Eliom_service.get , _, _, _, _,
       [ `WithoutSuffix ], unit, unit, 'a) Eliom_service.t ->
    'a redirection

module Redirection = struct

  type page = Eliom_service.non_ocaml redirection

  type options =
    [ `MovedPermanently
    | `Found
    | `SeeOther
    | `NotNodifed
    | `UseProxy
    | `TemporaryRedirect ]

  type return = Eliom_service.non_ocaml

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    Eliom_service.set_client_fun ?app ~service @@ fun g p ->
    lwt Redirection service = f g p in
    lwt () = Eliom_client.change_page service () () in
    Eliom_client.do_not_set_uri := true;
    Lwt.return ()

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
