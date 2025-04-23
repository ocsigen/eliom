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

let section = Lwt_log_js.Section.make "eliom:registration"

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
module String_redirection = Base
module Streamlist = Base

module Ocaml = struct
  type 'a return = 'a Eliom_service.ocaml
end

type 'a kind = Eliom_service.result
type browser_content = [ `Browser ]
type 'a application_content = [ `Appl of 'a ]

module type PARAM = sig
  type page
  type options
  type return
  type result

  val reset_reload_fun : bool
  val send : ?options:options -> page -> [ `Browser ] kind Lwt.t
end

let typed_apply ~service f gp pp l l' suffix =
  Lwt.catch
    (fun () ->
      let* g =
        let l = Some (Lwt.return l) in
        Eliom_parameter.reconstruct_params ~sp:() gp l None true suffix
      and* p =
        let l' = Some (Lwt.return l') in
        Eliom_parameter.reconstruct_params ~sp:() pp l' None true suffix
      in
      (match Eliom_service.reload_fun service with
      | Some _ -> Eliom_client.set_reload_function (fun () () -> f g p)
      | None -> ());
      f g p)
    (function
      | Eliom_common.Eliom_Wrong_parameter ->
          Lwt.fail Eliom_common.Eliom_Wrong_parameter
      | exc -> Lwt.reraise exc)

let wrap service att f _ suffix =
  let gp = Eliom_service.get_params_type service
  and pp = Eliom_service.post_params_type service
  and l = (Eliom_request_info.get_sess_info ()).si_all_get_but_nl
  and l' =
    match (Eliom_request_info.get_sess_info ()).si_all_post_params with
    | Some l -> l
    | None -> []
  in
  match Eliom_service.get_name att with
  | Eliom_common.SAtt_named s | Eliom_common.SAtt_anon s -> (
      try
        let eliom_name = List.assoc "__eliom__" l
        and l = List.remove_assoc "__eliom__" l
        and l' = List.remove_assoc "__eliom__" l' in
        if eliom_name = s then typed_apply ~service f gp pp l l' suffix
        else Lwt.fail Eliom_common.Eliom_Wrong_parameter
      with Not_found -> Lwt.fail Eliom_common.Eliom_Wrong_parameter)
  | _ -> typed_apply ~service f gp pp l l' suffix

let wrap_na (service : (_, _, _, _, _, _, _, _, _, _, _) Eliom_service.t)
    _non_att f _ suffix =
  let gp = Eliom_service.get_params_type service
  and pp = Eliom_service.post_params_type service
  and si = Eliom_request_info.get_sess_info ()
  and filter l = fst Eliom_common.(split_prefix_param na_co_param_prefix l) in
  let l = filter si.si_all_get_but_nl
  and l' = match si.si_all_post_params with Some l -> filter l | None -> [] in
  typed_apply ~service f gp pp l l' suffix

let register_att ~service ~att f =
  let key_meth = Eliom_service.which_meth_untyped service
  and gn = Eliom_service.get_name att
  and pn = Eliom_service.post_name att
  and priority = Eliom_service.priority att in
  let sgpt = Eliom_service.get_params_type service
  and sppt = Eliom_service.post_params_type service in
  (match Eliom_service.timeout service with
  | None -> ()
  | Some _ ->
      Lwt_log_js.ign_info ~section "Service timeout ignored on the client");
  let s_id =
    if gn = Eliom_common.SAtt_no || pn = Eliom_common.SAtt_no then
      Eliom_parameter.(anonymise_params_type sgpt, anonymise_params_type sppt)
    else (0, 0)
  and s_max_use = Eliom_service.max_use service
  and s_expire = None
  and s_f = wrap service att f in
  Eliom_route.add_service priority Eliom_route.global_tables
    (Eliom_service.sub_path att)
    {
      Eliom_common.key_state = (gn, pn);
      Eliom_common.key_meth :> Eliom_common.meth;
    }
    { s_id; s_max_use; s_expire; s_f }

let register_na ~service ~na f =
  Eliom_route.add_naservice
    Eliom_service.(na_name na)
    (wrap_na service na f) Eliom_route.global_tables

let register (type g p att)
    ~(service : (g, p, _, att, _, _, _, _, _, _, _) Eliom_service.t)
    (f : g -> p -> _) =
  match Eliom_service.info service with
  | Eliom_service.Attached att -> register_att ~service ~att f
  | Eliom_service.Nonattached na -> register_na ~service ~na f

module Make (P : PARAM) = struct
  type page = P.page
  type options = P.options
  type return = P.return
  type result = P.result

  let send ?options ?charset:_ ?code:_ ?content_type:_ ?headers:_ page =
    P.send ?options page

  let register ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ (type g p att)
      ~(service : (g, p, _, att, _, _, _, _, _, _, _) Eliom_service.t)
      ?error_handler:_ (f : g -> p -> _) =
    let f g p =
      let* page = f g p in
      P.send ?options page
    in
    register ~service f;
    Eliom_service.set_client_fun ?app ~service f;
    if P.reset_reload_fun then Eliom_service.reset_reload_fun service
end

module Html = Make (struct
  type page = Html_types.html Eliom_content.Html.elt
  type options = unit
  type return = Eliom_service.non_ocaml
  type result = browser_content kind

  let reset_reload_fun = false

  let send ?options:_ page =
    Lwt.return (Eliom_service.Dom (Eliom_content.Html.To_dom.of_element page))
end)

module Action = Make (struct
  type page = unit
  type options = [ `Reload | `NoReload ]
  type return = Eliom_service.non_ocaml
  type result = browser_content kind

  let reset_reload_fun = true

  let send ?options _page =
    match options with
    | Some `Reload | None ->
        Lwt.return
          Eliom_service.(Reload_action { hidden = false; https = false })
    | _ -> Lwt.return Eliom_service.No_contents
end)

module Unit = Make (struct
  type page = unit
  type options = unit
  type return = Eliom_service.non_ocaml
  type result = browser_content kind

  let reset_reload_fun = true
  let send ?options:_ _page = Lwt.return Eliom_service.No_contents
end)

type appl_service_options = { do_not_launch : bool }

module App (P : Eliom_registration_sigs.APP_PARAM) = struct
  type app_id

  let application_name = P.application_name

  include Make (struct
    type page = Html_types.html Eliom_content.Html.elt
    type options = appl_service_options
    type return = Eliom_service.non_ocaml
    type result = browser_content kind

    let reset_reload_fun = false

    let send ?options:_ page =
      Lwt.return (Eliom_service.Dom (Eliom_content.Html.To_dom.of_element page))
  end)
end

type 'a redirection =
  | Redirection :
      ( unit,
        unit,
        Eliom_service.get,
        _,
        _,
        _,
        _,
        [ `WithoutSuffix ],
        unit,
        unit,
        'a )
      Eliom_service.t
      -> 'a redirection

module Redirection = struct
  (* not really polymorphic; just adding a type variable to maintain
     type-level compatibility with server (for injections) *)
  type _ page = Eliom_service.non_ocaml redirection

  type options =
    [ `MovedPermanently
    | `Found
    | `SeeOther
    | `NotNodifed
    | `UseProxy
    | `TemporaryRedirect ]

  type _ return = Eliom_service.non_ocaml
  type _ result = browser_content kind

  let send ?options:_ ?charset:_ ?code:_ ?content_type:_ ?headers:_
      (Redirection service) =
    Lwt.return (Eliom_service.Redirect service)

  let register ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ (type g p att)
      ~(service : (g, p, _, att, _, _, _, _, _, _, _) Eliom_service.t)
      ?error_handler:_ (f : g -> p -> _) =
    let f g p =
      let* page = f g p in
      send ?options page
    in
    register ~service f;
    Eliom_service.set_client_fun ?app ~service f
end

module Any = struct
  type 'a page = 'a kind
  type 'a return = Eliom_service.non_ocaml
  type 'a result = 'a kind
  type options = unit

  let send ?options:_ ?charset:_ ?code:_ ?content_type:_ ?headers:_ page =
    Lwt.return page

  let register ?app ?scope:_ ?options:_ ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_ f =
    let f g p =
      let* page = f g p in
      send page
    in
    register ~service f;
    Eliom_service.set_client_fun ?app ~service f
end

let appl_self_redirect f x = f x

module String = Base
