open Lwt.Syntax

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

let section = Logs.Src.create "eliom:registration"

module type Base = sig
  type return = Service.non_ocaml
end

module Base = struct
  type return = Service.non_ocaml
end

module Block5 = Base
module Html_text = Base
module CssText = Base
module Text = Base
module String_redirection = Base
module Streamlist = Base

module Ocaml = struct
  type 'a return = 'a Service.ocaml
end

type 'a kind = Service.result
type browser_content = [`Browser]
type 'a application_content = [`Appl of 'a]

module type PARAM = sig
  type page
  type options
  type return
  type result

  val reset_reload_fun : bool
  val send : ?options:options -> page -> [`Browser] kind Lwt.t
end

let typed_apply ~service f gp pp l l' suffix =
  Lwt.catch
    (fun () ->
       let* g =
         let l = Some (Lwt.return l) in
         Parameter.reconstruct_params ~sp:() gp l None true suffix
       and* p =
         let l' = Some (Lwt.return l') in
         Parameter.reconstruct_params ~sp:() pp l' None true suffix
       in
       (match Service.reload_fun service with
       | Some _ -> Client.set_reload_function (fun () () -> f g p)
       | None -> ());
       f g p)
    (function
      | Common.Eliom_Wrong_parameter -> Lwt.fail Common.Eliom_Wrong_parameter
      | exc -> Lwt.reraise exc)

let wrap service att f _ suffix =
  let gp = Service.get_params_type service
  and pp = Service.post_params_type service
  and l = (Request_info.get_sess_info ()).si_all_get_but_nl
  and l' =
    match (Request_info.get_sess_info ()).si_all_post_params with
    | Some l -> l
    | None -> []
  in
  match Service.get_name att with
  | Common.SAtt_named s | Common.SAtt_anon s -> (
    try
      let eliom_name = List.assoc "__eliom__" l
      and l = List.remove_assoc "__eliom__" l
      and l' = List.remove_assoc "__eliom__" l' in
      if eliom_name = s
      then typed_apply ~service f gp pp l l' suffix
      else Lwt.fail Common.Eliom_Wrong_parameter
    with Not_found -> Lwt.fail Common.Eliom_Wrong_parameter)
  | _ -> typed_apply ~service f gp pp l l' suffix

let wrap_na
      (service : (_, _, _, _, _, _, _, _, _, _, _) Service.t)
      _non_att
      f
      _
      suffix
  =
  let gp = Service.get_params_type service
  and pp = Service.post_params_type service
  and si = Request_info.get_sess_info ()
  and filter l = fst Common.(split_prefix_param na_co_param_prefix l) in
  let l = filter si.si_all_get_but_nl
  and l' = match si.si_all_post_params with Some l -> filter l | None -> [] in
  typed_apply ~service f gp pp l l' suffix

let register_att ~service ~att f =
  let key_meth = Service.which_meth_untyped service
  and gn = Service.get_name att
  and pn = Service.post_name att
  and priority = Service.priority att in
  let sgpt = Service.get_params_type service
  and sppt = Service.post_params_type service in
  (match Service.timeout service with
  | None -> ()
  | Some _ ->
      Logs.info ~src:section (fun fmt ->
        fmt "Service timeout ignored on the client"));
  let s_id =
    if gn = Common.SAtt_no || pn = Common.SAtt_no
    then Parameter.(anonymise_params_type sgpt, anonymise_params_type sppt)
    else 0, 0
  and s_max_use = Service.max_use service
  and s_expire = None
  and s_f = wrap service att f in
  Route.add_service priority Route.global_tables (Service.sub_path att)
    {Common.key_state = gn, pn; Common.key_meth :> Common.meth}
    {s_id; s_max_use; s_expire; s_f}

let register_na ~service ~na f =
  Route.add_naservice
    Service.(na_name na)
    (wrap_na service na f) Route.global_tables

let register
      (type g p att)
      ~(service : (g, p, _, att, _, _, _, _, _, _, _) Service.t)
      (f : g -> p -> _)
  =
  match Service.info service with
  | Service.Attached att -> register_att ~service ~att f
  | Service.Nonattached na -> register_na ~service ~na f

module Make (P : PARAM) = struct
  type page = P.page
  type options = P.options
  type return = P.return
  type result = P.result

  let send ?options ?charset:_ ?code:_ ?content_type:_ ?headers:_ page =
    P.send ?options page

  let register
        ?app
        ?scope:_
        ?options
        ?charset:_
        ?code:_
        ?content_type:_
        ?headers:_
        ?secure_session:_
        (type g p att)
        ~(service : (g, p, _, att, _, _, _, _, _, _, _) Service.t)
        ?error_handler:_
        (f : g -> p -> _)
    =
    let f g p =
      let* page = f g p in
      P.send ?options page
    in
    register ~service f;
    Service.set_client_fun ?app ~service f;
    if P.reset_reload_fun then Service.reset_reload_fun service
end

module Html = Make (struct
    type page = Html_types.html Content.Html.elt
    type options = unit
    type return = Service.non_ocaml
    type result = browser_content kind

    let reset_reload_fun = false

    let send ?options:_ page =
      Lwt.return (Service.Dom (Content.Html.To_dom.of_element page))
  end)

module Action = Make (struct
    type page = unit
    type options = [`Reload | `NoReload]
    type return = Service.non_ocaml
    type result = browser_content kind

    let reset_reload_fun = true

    let send ?options _page =
      match options with
      | Some `Reload | None ->
          Lwt.return Service.(Reload_action {hidden = false; https = false})
      | _ -> Lwt.return Service.No_contents
  end)

module Unit = Make (struct
    type page = unit
    type options = unit
    type return = Service.non_ocaml
    type result = browser_content kind

    let reset_reload_fun = true
    let send ?options:_ _page = Lwt.return Service.No_contents
  end)

type appl_service_options = {do_not_launch : bool}

module App (P : Registration_sigs.APP_PARAM) = struct
  type app_id

  let application_name = P.application_name

  include Make (struct
      type page = Html_types.html Content.Html.elt
      type options = appl_service_options
      type return = Service.non_ocaml
      type result = browser_content kind

      let reset_reload_fun = false

      let send ?options:_ page =
        Lwt.return (Service.Dom (Content.Html.To_dom.of_element page))
    end)
end

type 'a redirection =
  | Redirection :
      ( unit
        , unit
        , Service.get
        , _
        , _
        , _
        , _
        , [`WithoutSuffix]
        , unit
        , unit
        , 'a )
        Service.t
      -> 'a redirection

module Redirection = struct
  (* not really polymorphic; just adding a type variable to maintain
     type-level compatibility with server (for injections) *)
  type _ page = Service.non_ocaml redirection

  type options =
    [ `MovedPermanently
    | `Found
    | `SeeOther
    | `NotNodifed
    | `UseProxy
    | `TemporaryRedirect ]

  type _ return = Service.non_ocaml
  type _ result = browser_content kind

  let send
        ?options:_
        ?charset:_
        ?code:_
        ?content_type:_
        ?headers:_
        (Redirection service)
    =
    Lwt.return (Service.Redirect service)

  let register
        ?app
        ?scope:_
        ?options
        ?charset:_
        ?code:_
        ?content_type:_
        ?headers:_
        ?secure_session:_
        (type g p att)
        ~(service : (g, p, _, att, _, _, _, _, _, _, _) Service.t)
        ?error_handler:_
        (f : g -> p -> _)
    =
    let f g p =
      let* page = f g p in
      send ?options page
    in
    register ~service f;
    Service.set_client_fun ?app ~service f
end

module Any = struct
  type 'a page = 'a kind
  type 'a return = Service.non_ocaml
  type 'a result = 'a kind
  type options = unit

  let send ?options:_ ?charset:_ ?code:_ ?content_type:_ ?headers:_ page =
    Lwt.return page

  let register
        ?app
        ?scope:_
        ?options:_
        ?charset:_
        ?code:_
        ?content_type:_
        ?headers:_
        ?secure_session:_
        ~service
        ?error_handler:_
        f
    =
    let f g p =
      let* page = f g p in
      send page
    in
    register ~service f;
    Service.set_client_fun ?app ~service f
end

let appl_self_redirect f x = f x

module String = Base
