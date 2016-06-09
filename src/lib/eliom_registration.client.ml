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

module Streamlist = Base

module Ocaml = struct
  type 'a return = 'a Eliom_service.ocaml
end

type 'a kind = unit
type browser_content = [`Browser]

module type PARAM = sig

  type page
  type options
  type return
  type result

  val send : ?options:options -> page -> unit Lwt.t

end

module Make (P : PARAM) = struct

  type page = P.page
  type options = P.options
  type return = P.return
  type result = P.result

  let send ?options ?charset ?code ?content_type ?headers page =
    P.send ?options page

  let wrap service f _ _ =
    let gp = Eliom_service.get_params_type service in
    lwt g =
      Eliom_parameter.reconstruct_params
        ~sp:() gp None None true None
    in
    f g ()

  let register_service_attached ~service ~att f =
    let key_meth = Eliom_service.which_meth_untyped service in
    let gn = Eliom_service.get_name att in
    let pn = Eliom_service.post_name att in
    let suffix_with_redirect = Eliom_service.redirect_suffix att in
    let priority = Eliom_service.priority att in
    let sgpt = Eliom_service. get_params_type service in
    let sppt = Eliom_service.post_params_type service in
    Eliom_route.add_service
      priority
      Eliom_route.global_tables
      (Eliom_service.sub_path att)
      {Eliom_common.key_state = (gn, pn);
       Eliom_common.key_meth = (key_meth :> Eliom_common.meth)}
      ((if gn = Eliom_common.SAtt_no || pn = Eliom_common.SAtt_no then
          Eliom_parameter.(
            anonymise_params_type sgpt,
            anonymise_params_type sppt
          )
        else
          (0, 0)
       ),
       ((match Eliom_service.max_use service with
          | None -> None
          | Some i -> Some (ref i)),
        (match Eliom_service.timeout service with
         | None -> None
         | Some t -> Some (t, ref (t +. Unix.time ()))),
        wrap service f))

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_
      (type g) (type p) (type att)
      ~(service : (g, p, _, att, _, _, _, _, _, _, _) Eliom_service.t)
      ?error_handler:_
      (f : g -> p -> _) =
    let f g p = lwt page = f g p in P.send ?options page in
    (match
       Eliom_service.info service,
       Eliom_parameter.is_unit (Eliom_service.post_params_type service)
     with
     | Eliom_service.Attached att, Eliom_parameter.U_yes ->
       register_service_attached ~service ~att f
     | _ ->
       ());
    Eliom_service.set_client_fun ?app ~service f

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

module Html = Make (struct

    type page = Html_types.html Eliom_content.Html.elt
    type options = unit
    type return = Eliom_service.non_ocaml
    type result = browser_content kind

    let send ?options:_ page =
      Eliom_client.set_content_local
        (Eliom_content.Html.To_dom.of_element page)

  end)

module Action = struct

  type page = unit
  type options = [`Reload | `NoReload]
  type return = Eliom_service.non_ocaml
  type result = browser_content kind

  let send ?options ?charset:_ ?code:_ ?content_type:_ ?headers:_ page =
    Eliom_client.do_not_set_uri := true;
    match !Eliom_client.reload_function, options with
    | Some rf, (Some `Reload | None) ->
      rf () ()
    | None, (Some `Reload | None) ->
      (* last page was probably generated on the server, so
         default a standard reload *)
      Dom_html.window##location##reload();
      Lwt.return ()
    | _, Some `NoReload ->
      Lwt.return ()

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    Eliom_service.set_client_fun ?app ~service
      (fun g p -> lwt page = f g p in send ?options page);
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

module Redirection = Make (struct

    type page = Eliom_service.non_ocaml redirection

    type options =
      [ `MovedPermanently
      | `Found
      | `SeeOther
      | `NotNodifed
      | `UseProxy
      | `TemporaryRedirect ]

    type return = Eliom_service.non_ocaml

    type result = browser_content kind

    let send ?options:_ (Redirection service) =
      lwt () = Eliom_client.change_page service () () in
      Eliom_client.do_not_set_uri := true;
      Lwt.return ()

  end)

module Any = struct

  type 'a page = 'a kind
  type 'a return = Eliom_service.non_ocaml
  type 'a result = 'a kind
  type options = unit

  let send ?options:_ ?charset:_ ?code:_ ?content_type:_ ?headers:_ page =
    Lwt.return page

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    Eliom_service.set_client_fun ?app ~service
      (fun g p -> lwt page = f g p in send page)

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
