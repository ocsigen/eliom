(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
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

include Eliom_service_base

let attach :
  fallback:
  (unit, unit, get, att, _, non_ext, 'rg1,
   [< suff ], unit, unit, 'return1) t ->
  service:
  ('get, 'post, 'gp, non_att, co, non_ext, 'rg2,
   [< `WithoutSuffix] as 'sf, 'gn, 'pn, 'return) t ->
  unit ->
  ('get, 'post, 'gp, att, co, non_ext, non_reg,
   'sf, 'gn, 'pn, 'return) t =
  fun ~fallback ~service () ->
    let {na_name} = non_attached_info service in
    let fallbackkind = attached_info fallback in
    let open Eliom_common in
    let error_msg =
      "attach' is not implemented for this kind of\
       service. Please report a bug if you need this."
    in
    let get_name = match na_name with
      | SNa_get_ s -> SAtt_na_named s
      | SNa_get' s -> SAtt_na_anon s
      | SNa_get_csrf_safe a -> SAtt_na_csrf_safe a
      | SNa_post_ s -> fallbackkind.get_name (*VVV check *)
      | SNa_post' s -> fallbackkind.get_name (*VVV check *)
      | SNa_post_csrf_safe a -> fallbackkind.get_name (*VVV check *)
      | _ -> failwith error_msg
    (*VVV Do we want to make possible to attach POST na coservices
          on GET attached coservices? *)
    and post_name = match na_name with
      | SNa_get_ s -> SAtt_no
      | SNa_get' s -> SAtt_no
      | SNa_get_csrf_safe a -> SAtt_no
      | SNa_post_ s -> SAtt_na_named s
      | SNa_post' s -> SAtt_na_anon s
      | SNa_post_csrf_safe a -> SAtt_na_csrf_safe a
      | _ -> failwith error_msg
    in {
      service with
      service_mark = service_mark ();
      kind = `AttachedCoservice;
      pre_applied_parameters = fallback.pre_applied_parameters;
      info = Attached {fallbackkind with get_name ; post_name }
    }

let xhr_with_cookies s =
  if is_external s then
    None
  else
    match s.send_appl_content with
    | XAlways -> Some None
    | XNever -> None
    | XSame_appl (appl, _) when appl <> Eliom_process.get_application_name () ->
      None
    | XSame_appl (_, tmpl) -> Some tmpl

let client_fun service =
  match service.client_fun with
  | Some f -> !f
  | None   -> None

let has_client_fun service = client_fun service <> None

let set_client_fun ?app ~service f =
  Eliom_lib.Option.iter
    (fun name -> service.send_appl_content <- XSame_appl (name, None))
    app;
  match service.client_fun with
  | Some r -> r := Some f
  | None   -> service.client_fun <- Some (ref (Some f))

let reload_fun :
  type gp pp .
  (gp, pp, _, _, _, _, _, _, _, _, _) t ->
  (gp -> unit -> result Lwt.t) option =
  fun service ->
    match Eliom_parameter.is_unit (post_params_type service) with
    | Eliom_parameter.U_yes ->
      (match service with
       | { client_fun = Some {contents = Some f} ;
           reload_fun = Rf_client_fun } ->
         Some f
       | _ ->
         None)
    | _ ->
      None

let reset_reload_fun service = service.reload_fun <- Rf_keep

let register_delayed_get_or_na_coservice ~sp s =
  failwith "CSRF coservice not implemented client side for now"

let register_delayed_post_coservice  ~sp s getname =
  failwith "CSRF coservice not implemented client side for now"
