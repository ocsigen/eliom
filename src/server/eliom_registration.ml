(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_registration
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

open Eliom_lib
open Eliom_lib.Lwt_ops
open Eliom_content
open Eliom_service
open Eliom_parameter

let code_of_code_option = function
  | None -> 200
  | Some c -> c

include Eliom_registration_base

(******************************************************************************)
(* Send return types                                                          *)
(******************************************************************************)

module Result_types :
sig
  type 'a kind
  val cast_result : Ocsigen_http_frame.result -> 'a kind
  val cast_kind : 'a kind -> Ocsigen_http_frame.result
  val cast_kind_lwt : 'a kind Lwt.t -> Ocsigen_http_frame.result Lwt.t
  val cast_result_lwt : Ocsigen_http_frame.result Lwt.t -> 'a kind Lwt.t
  val cast_function_kind : ('c -> 'a kind Lwt.t) -> ('c -> 'd kind Lwt.t)
  val cast_function_http : ('c -> 'a kind Lwt.t) -> ('c -> Ocsigen_http_frame.result Lwt.t)
end
=
struct
  type 'a kind = Ocsigen_http_frame.result
  let cast_result x = x
  let cast_kind x = x
  let cast_kind_lwt x = x
  let cast_result_lwt x = x
  let cast_function_kind x = x
  let cast_function_http x = x
end

type 'a kind = 'a Result_types.kind
type 'a application_content = [`Appl of 'a]
type block_content
type browser_content = [`Browser]
type 'a caml_content
type unknown_content

let cast_unknown_content_kind (x:unknown_content kind) : 'a kind =
  Result_types.cast_result (Result_types.cast_kind x)
let cast_http_result = Result_types.cast_result

(******************************************************************************)

(******************************************************************************)

module Html5_make_reg_base
  (Html5_content : Ocsigen_http_frame.HTTP_CONTENT
                   with type t = Html5_types.html Eliom_content.Html5.elt
                   and type options = Http_headers.accept Lazy.t)
  = struct

  open Eliom_content.Html5.F
  open Html5_types

  type page = xhtml elt

  type options = unit

  type return = http_service

  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send
      ?(options = ()) ?charset ?code
      ?content_type ?headers content =
    let accept =
      (Eliom_request_info.get_ri ()).Ocsigen_extensions.ri_accept in
    lwt r = Html5_content.result_of_content ~options:accept content in
    let open Ocsigen_http_frame in
    Lwt.return
      {r with
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset
                      );
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults headers r.res_headers
                      );
      }

end



module Html5_reg_base =
  Html5_make_reg_base(Ocsigen_senders.Make_XML_Content(Xml)(Eliom_content.Html5.D))

module Html5_registration = Eliom_mkreg.MakeRegister(Html5_reg_base)

module Html5 = struct
  include Html5_registration
end

(******************************************************************************)
(******************************************************************************)


(****************************************************************************)
(****************************************************************************)

module Make_typed_xml_registration
  (Xml: Xml_sigs.Iterable)
  (Typed_xml: Xml_sigs.Typed_xml with module Xml := Xml)
  (E : sig type content end) = struct

    module Format = Xml_print.Make_typed(Xml)(Typed_xml)(Ocsigen_stream.StringStream)

    let result_of_content_subxhtml get_etag c =
      let x = Format.print_list c in
      let default_result = Ocsigen_http_frame.default_result () in
      Lwt.return
        {default_result with
          Ocsigen_http_frame.res_content_length = None;
          res_content_type = Some "text/html";
          res_etag = get_etag c;
          res_headers= Http_headers.dyn_headers;
          res_stream = (x, None)
        }

    module Cont_content =
      (* Pasted from ocsigen_senders.ml and modified *)
      struct
        type t = E.content Typed_xml.elt list

        let get_etag_aux x = None

        let get_etag ?options c = None

        let result_of_content c = result_of_content_subxhtml get_etag c

      end

    module Cont_reg_base = struct

      type page = E.content Typed_xml.elt list

      type options = unit

      type return = http_service

      type result = block_content kind

      let result_of_http_result = Result_types.cast_result

      let send_appl_content = Eliom_service.XNever

      let send ?options ?charset ?code ?content_type ?headers content =
        lwt r = Cont_content.result_of_content content in
        Lwt.return
          { r with
            Ocsigen_http_frame.
            res_code    = code_of_code_option code;
            res_charset = (match charset with
              | None -> Some (Eliom_config.get_config_default_charset ())
              | _ -> charset);
            res_content_type = (match content_type with
              | None -> r.Ocsigen_http_frame.res_content_type
              | _ -> content_type
            );
            res_headers = (match headers with
              | None -> r.Ocsigen_http_frame.res_headers
              | Some headers ->
                Http_headers.with_defaults
                  headers r.Ocsigen_http_frame.res_headers
            );
          }

    end

    include Eliom_mkreg.MakeRegister(Cont_reg_base)

  end

module Flow5 = Make_typed_xml_registration(Xml)(Eliom_content.Html5.D)(struct
  type content = Html5_types.flow5
end)


(****************************************************************************)
(****************************************************************************)

let (<-<) h (n, v) = Http_headers.replace n v h
let add_cache_header cache headers =
  match cache with
    | None -> headers
    | Some 0 ->
      headers
      <-< (Http_headers.cache_control, "no-cache")
      <-< (Http_headers.expires, "0")
    | Some duration ->
      headers
      <-< (Http_headers.cache_control, "max-age: "^ string_of_int duration)
      <-< (Http_headers.expires,
           Ocsigen_http_com.gmtdate (Unix.time () +. float_of_int duration))


module Text_reg_base = struct

  type page = (string * string)

  type options = int

  type return = http_service

  type result = unknown_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r = Ocsigen_senders.Text_content.result_of_content content in
    let res_headers = match headers with
      | None -> r.Ocsigen_http_frame.res_headers
      | Some headers ->
        Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers in
    let res_headers = add_cache_header options res_headers in
    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None ->  Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type = (match content_type with
          | None -> r.Ocsigen_http_frame.res_content_type
          | _ -> content_type
        );
        res_headers;
      }

end

module Text = Eliom_mkreg.MakeRegister(Text_reg_base)

(****************************************************************************)
(****************************************************************************)

module CssText_reg_base = struct

  type page = string

  type options = int

  type return = http_service

  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r =
      Ocsigen_senders.Text_content.result_of_content (content, "text/css") in
    let res_headers = match headers with
      | None -> r.Ocsigen_http_frame.res_headers
      | Some headers ->
        Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers in
    let res_headers = add_cache_header options res_headers in
    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type = (match content_type with
          | None -> r.Ocsigen_http_frame.res_content_type
          | _ -> content_type
        );
        res_headers;
      }

end

module CssText = Eliom_mkreg.MakeRegister(CssText_reg_base)

(****************************************************************************)
(****************************************************************************)

module HtmlText_reg_base = struct

  type page = string

  type options = unit

  type return = http_service

  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt r =
      Ocsigen_senders.Text_content.result_of_content (content, "text/html") in
    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset);
        res_content_type = (match content_type with
          | None -> r.Ocsigen_http_frame.res_content_type
          | _ -> content_type
        );
        res_headers = (match headers with
          | None -> r.Ocsigen_http_frame.res_headers
          | Some headers ->
            Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
        );
      }

end


module HtmlText_registration = Eliom_mkreg.MakeRegister(HtmlText_reg_base)

module Html_text = struct
  include HtmlText_registration
end

(****************************************************************************)
(****************************************************************************)

(** Actions are like services, but do not generate any page. The current
   page is reloaded (but if you give the optional parameter
    [~options:`NoReload] to the registration function).
 *)
module Action_reg_base = struct

  type page = unit

  type options = [ `Reload | `NoReload ]

  type return = http_service

  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways
  (* The post action service will decide later *)

  let send_directly =
  (* send bypassing the following directives
     in the configuration file (they have already been taken into account) *)
    fun ri res ->
      Polytables.set
        ri.Ocsigen_extensions.ri_request_cache Eliom_common.found_stop_key ();
      res

  let send
      ?(options = `Reload) ?charset ?(code = 204)
      ?content_type ?headers () =
    let user_cookies = Eliom_request_info.get_user_cookies () in
    if options = `NoReload
    then
      let open Ocsigen_http_frame in
      let empty_result = Ocsigen_http_frame.empty_result () in
      let h = match headers with
        | None -> empty_result.res_headers
        | Some headers ->
          Http_headers.with_defaults headers empty_result.res_headers
      in
      let h =
        match Eliom_request_info.get_sp_client_appl_name () with
          | Some anr ->
            Http_headers.replace
              (Http_headers.name Eliom_common_base.appl_name_header_name)
              anr
              h
          | _ -> h
      in
      Lwt.return
        {empty_result with
          res_code= code;
          res_content_type= (match content_type with
            | None -> empty_result.res_content_type
            | _ -> content_type
          );
          res_headers= h
        }
    else
      (* It is an action, we reload the page.
         To do that, we retry without POST params.
         If no post param at all, we retry
         without GET non_att info.
         If no GET non_att info, we retry without
         GET state.
         If no GET state,
         we do not reload, otherwise it will loop.
      *)
      (* be very careful while re-reading this *)
      let sp = Eliom_common.get_sp () in
      let sitedata = Eliom_request_info.get_sitedata_sp sp in
      let si = Eliom_request_info.get_si sp in
      let ri = Eliom_request_info.get_request_sp sp in
      let open Ocsigen_extensions in
      let open Ocsigen_http_frame in
      match (si.Eliom_common.si_nonatt_info,
                       si.Eliom_common.si_state_info,
                       ri.request_info.ri_method) with
        | (Eliom_common.RNa_no,
           (Eliom_common.RAtt_no, Eliom_common.RAtt_no),
           Ocsigen_http_frame.Http_header.GET) ->
          let empty_result = Ocsigen_http_frame.empty_result () in
          Lwt.return empty_result
        | _ ->
          let all_cookie_info = sp.Eliom_common.sp_cookie_info in
          lwt ric = Eliommod_cookies.compute_new_ri_cookies
            (Unix.time ())
            ri.request_info.ri_sub_path
            (Lazy.force ri.request_info.ri_cookies)
            all_cookie_info
            user_cookies
          in
          lwt all_new_cookies =
            Eliommod_cookies.compute_cookies_to_send
              sitedata
              all_cookie_info
              user_cookies in

          (* Now tab cookies:
             As tab cookies are sent only by Eliom_app services,
             we just need to keep them in rc.
             If the fallback service is not Eliom_app, they will
             be lost.
          *)
          let rc = Eliom_request_info.get_request_cache_sp sp in
          Polytables.set
            ~table:rc
            ~key:Eliom_common.tab_cookie_action_info_key
            ~value:(sp.Eliom_common.sp_tab_cookie_info,
                    sp.Eliom_common.sp_user_tab_cookies,
                    si.Eliom_common.si_tab_cookies
            );

          (* Now removing some parameters to decide the following service: *)
          match (si.Eliom_common.si_nonatt_info,
                 si.Eliom_common.si_state_info,
                 ri.request_info.ri_method) with
            | (Eliom_common.RNa_get_ _,
               (_, Eliom_common.RAtt_no),
               Ocsigen_http_frame.Http_header.GET)
            | (Eliom_common.RNa_get' _,
               (_, Eliom_common.RAtt_no),
               Ocsigen_http_frame.Http_header.GET)
            (* no post params, GET na coservice *)
            | (Eliom_common.RNa_no,
               (_, Eliom_common.RAtt_no),
               Ocsigen_http_frame.Http_header.GET)
              (* no post params, GET attached coservice *)
              ->
              Polytables.set
                ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                Eliom_common.eliom_params_after_action
                (si.Eliom_common.si_all_get_params,
                 si.Eliom_common.si_all_post_params, (* is Some [] *)
                 si.Eliom_common.si_nl_get_params,
                 si.Eliom_common.si_nl_post_params,
                 si.Eliom_common.si_all_get_but_nl);
                (*VVV Also put all_cookie_info in this,
                  to avoid update_cookie_table and get_cookie_info (?)
                *)
                let ri =
                  { ri.request_info with
                    ri_cookies= lazy ric;
                    ri_get_params =
                      lazy si.Eliom_common.si_other_get_params;
                  (* Here we modify ri,
                     thus the request can be taken by other extensions,
                     with its new parameters *)
                  }
                in
                lwt () = Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
                send_directly ri (Ocsigen_extensions.compute_result
                                    ~previous_cookies:all_new_cookies ri)

            | (Eliom_common.RNa_post_ _, (_, _), _)
            | (Eliom_common.RNa_post' _, (_, _), _) ->
              (* POST na coservice *)
              (* retry without POST params *)

              Polytables.set
                ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                Eliom_common.eliom_params_after_action
                (si.Eliom_common.si_all_get_params,
                 si.Eliom_common.si_all_post_params,
                 si.Eliom_common.si_nl_get_params,
                 si.Eliom_common.si_nl_post_params,
                 si.Eliom_common.si_all_get_but_nl);
              let ri =
                { ri.request_info with
                  ri_method = Ocsigen_http_frame.Http_header.GET;
                  ri_cookies= lazy ric;
                  ri_get_params =
                    lazy si.Eliom_common.si_other_get_params;
                  ri_post_params = Some (fun _ -> Lwt.return []);
                  ri_files = Some (fun _ -> Lwt.return []);
                }
              in
              lwt () = Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
              send_directly ri (Ocsigen_extensions.compute_result
                                  ~previous_cookies:all_new_cookies ri)

            | _ ->
              (* retry without POST params *)
              (*VVV
                Warning: is it possible to have an Eliom service with POST method
                but no POST parameter?
                --> may loop...
                (we impose GET to prevent that)
              *)
              Polytables.set
                ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                Eliom_common.eliom_params_after_action
                (si.Eliom_common.si_all_get_params,
                 si.Eliom_common.si_all_post_params,
                 si.Eliom_common.si_nl_get_params,
                 si.Eliom_common.si_nl_post_params,
                 si.Eliom_common.si_all_get_but_nl);
              let ri =
                { ri.request_info with
                  ri_method = Ocsigen_http_frame.Http_header.GET;
                  ri_cookies= lazy ric;
                  ri_get_params =
                    lazy si.Eliom_common.si_other_get_params;
                  ri_post_params = Some (fun _ -> Lwt.return []);
                  ri_files = Some (fun _ -> Lwt.return []);
                }
              in
              lwt () =
                Eliommod_pagegen.update_cookie_table sitedata all_cookie_info in
              send_directly ri (Ocsigen_extensions.compute_result
                                  ~previous_cookies:all_new_cookies ri)

end

module Action = Eliom_mkreg.MakeRegister(Action_reg_base)

(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all.
    (Same as {!Action} with [`NoReload] option).
 *)

module Unit_reg_base = struct

  type page = unit
  type options = unit
  type return = http_service
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?(code = 204)
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    let empty_result = Ocsigen_http_frame.empty_result () in
    Lwt.return
      {empty_result with
         res_code= code;
         res_content_type= (match content_type with
                              | None -> empty_result.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> empty_result.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers empty_result.res_headers
                      );
      }

end

module Unit = Eliom_mkreg.MakeRegister(Unit_reg_base)

(*****************************************************************************)
(*****************************************************************************)

(* Any is a module allowing to register services that decide themselves
   what they want to send.
 *)
module Any_reg_base = struct

  type ('a, 'b) page = 'a kind
  type options = unit
  type 'a return = 'a
  type 'a result = 'a kind

  let result_of_http_result = Result_types.cast_result

(*  let send_appl_content = Eliom_service.XNever *)
  let send_appl_content = Eliom_service.XAlways

  let send ?options ?charset ?code
      ?content_type ?headers (res:'a kind) =
    let res = Result_types.cast_kind res in
    let open Ocsigen_http_frame in
    Lwt.return
      {res with
         res_charset= (match charset with
                         | None -> res.res_charset
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> res.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> res.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers res.res_headers
                      );
      }

end

module Any = Eliom_mkreg.MakeRegister_AlphaReturn(Any_reg_base)

type 'a application_name = string

let appl_self_redirect send page =
  let open Ocsigen_http_frame in
      if Eliom_request_info.expecting_process_page ()
      then
        let url = Eliom_request_info.get_full_url () in
        let empty_result = empty_result () in
        Lwt.return
          (Result_types.cast_result {empty_result with
            res_headers=
              Http_headers.add
                (Http_headers.name Eliom_common.half_xhr_redir_header) url
                empty_result.res_headers})
      else
        lwt r = (Result_types.cast_function_http send) page in
        Lwt.return (Result_types.cast_result r)

let http_redirect = appl_self_redirect

(*****************************************************************************)
(*****************************************************************************)

(* File is a module allowing to register services that send files *)
module File_reg_base = struct

  type page = string
  type options = int
  type return = http_service
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    let file =
      try Ocsigen_local_files.resolve request filename ()
      with
        | Ocsigen_local_files.Failed_403 (* XXXBY : maybe we should signal a true 403? *)
        | Ocsigen_local_files.Failed_404
        | Ocsigen_local_files.NotReadableDirectory ->
            raise Eliom_common.Eliom_404
    in
    lwt r = Ocsigen_local_files.content ~request ~file in
    let open Ocsigen_http_frame in
    let open Ocsigen_extensions in
    let res_headers = match headers with
      | None -> r.res_headers
      | Some headers -> Http_headers.with_defaults headers r.res_headers
    in
    let res_headers = add_cache_header options res_headers in
    Lwt.return
      { r with
          res_code = code_of_code_option code;
          res_charset = (match charset with
                           | None ->
                               Some (Ocsigen_charset_mime.find_charset
                                       filename
                                       (Eliom_config.get_config_info_sp sp).charset_assoc)
                           | _ -> charset);
          res_content_type= (match content_type with
                               | None -> r.res_content_type
                               | _ -> content_type
                            );
          res_headers;

      }

end

module File =
struct
  include Eliom_mkreg.MakeRegister(File_reg_base)
  let check_file filename =
    let sp = Eliom_common.get_sp () in
    let request = Eliom_request_info.get_request_sp sp in
    try
      ignore (Ocsigen_local_files.resolve request filename ()
                : Ocsigen_local_files.resolved);
      true
    with
      | Ocsigen_local_files.Failed_403
      | Ocsigen_local_files.Failed_404
      | Ocsigen_local_files.NotReadableDirectory ->
        false
end

(****************************************************************************)
(****************************************************************************)

module Streamlist_reg_base = struct

  type page = (((unit -> (string Ocsigen_stream.t) Lwt.t) list) * string)
  type options = unit
  type return = http_service
  type result = unknown_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    let open Ocsigen_http_frame in
    Ocsigen_senders.Streamlist_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (Eliom_config.get_config_default_charset ())
                         | _ -> charset);
         res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                           );
         res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers r.res_headers
                      );
      }

end

module Streamlist = Eliom_mkreg.MakeRegister(Streamlist_reg_base)

(****************************************************************************)
(****************************************************************************)

module type Registration = sig
  type page
  type options
  type return
  type result
  include "sigs/eliom_reg_simpl.mli"
  subst type returnB := return
  and type returnT := return
end

module Customize
  (R : Registration)
  (T : sig type page val translate : page -> R.page Lwt.t end) = struct

    type page = T.page
    type return = R.return
    type options = R.options
    type result = R.result

  let make_eh = function
    | None -> None
    | Some eh -> Some (fun l -> eh l >>= T.translate)

  let make_service_handler f g p = f g p >>= T.translate

  let send ?options ?charset ?code ?content_type ?headers content =
    T.translate content >>= fun c ->
    R.send ?options ?charset ?code ?content_type ?headers c

  let register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~service
      ?error_handler
      (f : ('get -> 'post -> 'return Lwt.t)) =
    R.register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~service
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?priority
      ~path
      ~get_params
      ?error_handler
      f =
    R.register_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?priority
      ~path
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    R.register_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback:(Eliom_service.untype_service_ fallback)
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    R.register_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_post_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?priority
      ~fallback
      ~post_params
      ?error_handler
      f =
    R.register_post_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?priority
      ~fallback:(Eliom_service.untype_service_ fallback)
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_post_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    R.register_post_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback:(Eliom_service.untype_service_ fallback)
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_post_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    R.register_post_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

end

(****************************************************************************)
(****************************************************************************)

module Caml_reg_base = struct

  type page = string
  type options = unit
  type return = http_service
  type result = Ocsigen_http_frame.result

  let result_of_http_result x = x

  let send_appl_content = Eliom_service.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Result_types.cast_kind_lwt
      (Text.send ?charset ?code
         ?content_type ?headers
         (content,
          Eliom_service.eliom_appl_answer_content_type))

end

module Ocaml = struct

  module M = Eliom_mkreg.MakeRegister(Caml_reg_base)

  let prepare_data data =
    let ecs_request_data = Eliom_service.get_request_data () in
(*     debug_client_value_data (debug "%s") client_value_data; *)
    let r = { Eliom_types.
              ecs_request_data;
              ecs_data = data } in
    Lwt.return (Eliom_types.encode_eliom_data r)

  let make_eh = function
    | None -> None
    | Some eh ->
        Some (fun l -> eh l >>= prepare_data)

  let make_service_handler f =
    fun g p ->
      lwt data =
        try_lwt
          lwt res = f g p in
          Lwt.return (`Success res)
        with exc ->
          Lwt.return (`Failure (Printexc.to_string exc))
      in
      prepare_data data

  let send ?options ?charset ?code ?content_type ?headers content =
    lwt content = prepare_data content in
    Result_types.cast_result_lwt
      (M.send ?options ?charset ?code
         ?content_type ?headers content)

  let register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~(service : ('get, 'post,
                   [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ],
                   'return Eliom_service.caml_service) service)
      ?(error_handler : ((string * exn) list -> 'return Lwt.t) option)
      (f : ('get -> 'post -> 'return Lwt.t)) =
    M.register
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ~service:(Eliom_service.untype_service_ service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)


  let register_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?priority
      ~path
      ~get_params
      ?error_handler
      f =
    Eliom_service.untype_service_ (M.register_service
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?secure_session
                                      ?https
                                      ?priority
                                      ~path
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    Eliom_service.untype_service_ (M.register_coservice
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_service.untype_service_ fallback)
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    Eliom_service.untype_service_ (M.register_coservice'
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))


  let register_post_service
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?https
      ?priority
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_service.untype_service_ (M.register_post_service
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?secure_session
                                      ?https
                                      ?priority
                                      ~fallback:(Eliom_service.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_post_coservice
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_service.untype_service_ (M.register_post_coservice
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_service.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_post_coservice'
      ?scope
      ?options
      ?charset
      ?code
      ?content_type
      ?headers
      ?secure_session
      ?name
      ?csrf_safe
      ?csrf_scope
      ?csrf_secure
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    Eliom_service.untype_service_ (M.register_post_coservice'
                                      ?scope
                                      ?options
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?secure_session
                                      ?name
                                      ?csrf_safe
                                      ?csrf_scope
                                      ?csrf_secure
                                      ?max_use
                                      ?timeout
                                      ?keep_get_na_params
                                      ?https
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))


end

(****************************************************************************)
(****************************************************************************)

type appl_service_options =
    {
      do_not_launch : bool; (** Do not launch the client side program
                                       if it is not already launched.
                                       Default: [false]. *)
    }

let default_appl_service_options = {do_not_launch = false; }

module type APPL_PARAMS = sig
     val application_name : string
end

let comet_service_key = Polytables.make_key ()

let request_template =
  Eliom_reference.eref ~scope:Eliom_common.request_scope None

module Eliom_appl_reg_make_param
  (Html5_content
     : Ocsigen_http_frame.HTTP_CONTENT
       with type t = [ `Html ] Eliom_content.Html5.elt
       and type options = Http_headers.accept Lazy.t)
  (Appl_params : APPL_PARAMS) = struct

  open Eliom_content.Html5.D
  open Html5_types

  type appl

  type page = html elt
  type options = appl_service_options
  type return = appl_service
  type result = appl application_content kind

  let result_of_http_result = Result_types.cast_result

  let is_initial_request () =
    let sp = Eliom_common.get_sp () in
    sp.Eliom_common.sp_client_appl_name <> Some Appl_params.application_name

  let eliom_appl_script_id : [ `Script ] Eliom_content.Html5.Id.id =
    Eliom_content.Html5.Id.new_elt_id ~global:true ()
  let application_script ?(async = false) () =
    Eliom_content.Html5.Id.create_named_elt
      ~id:eliom_appl_script_id
      (Eliom_content.Html5.D.js_script
	 ~a:(if async then [Eliom_content.Html5.D.a_async `Async] else [] )
	 ~uri:(Eliom_content.Html5.D.make_uri
		 ~service:(Eliom_service.static_dir ())
		 [Appl_params.application_name ^ ".js"])
	 ())
  let application_script =
    (application_script
     : ?async:_ -> unit -> [ `Script ] Eliom_content.Html5.elt
     :> ?async:_ -> unit -> [> `Script ] Eliom_content.Html5.elt)
  let is_eliom_appl_script elt =
    Eliom_content.Html5.Id.have_id eliom_appl_script_id elt

  let eliom_appl_data_script_id =
    Eliom_content.Html5.Id.new_elt_id ~global:true ()

  let make_eliom_appl_data_script ~sp =

    let script =
      Printf.sprintf
        "var __eliom_appl_sitedata = \'%s\';\n\
         var __eliom_appl_process_info = \'%s\'\n\
         var __eliom_request_data;\n\
         var __eliom_request_cookies;\n\
         var __eliom_request_template;\n"
        (Eliom_lib.jsmarshal (Eliommod_cli.client_sitedata sp))
        (Eliom_lib.jsmarshal (sp.Eliom_common.sp_client_process_info))
    in

    Lwt.return
      (Eliom_content.Html5.Id.create_named_elt ~id:eliom_appl_data_script_id
	 (Eliom_content.Html5.F.script (cdata_script script)))

  let make_eliom_data_script ~sp page =

    let ejs_global_data =
      if is_initial_request () then
        Some (Eliom_service.get_global_data (), global_data_unwrapper)
      else None
    in
    let ejs_request_data = Eliom_service.get_request_data () in

(*
    debug "Global data %s"
      (match ejs_global_data with
         | Some (global_data, _) -> global_data_to_string global_data
         | None -> "-/-");
    debug "Request data %s"
      (request_data_to_string ejs_request_data);
 *)

    (* wrapping of values could create eliom references that may
       create cookies that needs to be sent along the page. Hence,
       cookies should be calculated after wrapping. *)
    let eliom_data =
      Eliom_content.Xml.wrap (Eliom_content.Html5.D.toelt page) { Eliom_common.
        ejs_global_data;
        ejs_request_data;
        ejs_event_handler_table = Eliom_content.Xml.make_event_handler_table (Eliom_content.Html5.D.toelt page);
        ejs_sess_info           = Eliommod_cli.client_si sp.Eliom_common.sp_si;
      } in

    lwt tab_cookies =
      Eliommod_cookies.compute_cookies_to_send
        sp.Eliom_common.sp_sitedata
        sp.Eliom_common.sp_tab_cookie_info
        sp.Eliom_common.sp_user_tab_cookies
    in

    lwt template = Eliom_reference.get request_template in

    let script =
      Printf.sprintf
        "__eliom_request_data = \'%s\';\n\
         __eliom_request_cookies = \'%s\';\n\
         __eliom_request_template = \'%s\';"
        (Eliom_lib.jsmarshal eliom_data)
        (Eliom_lib.jsmarshal tab_cookies)
        (Eliom_lib.jsmarshal (template: string option))
    in
    Lwt.return (Eliom_content.Html5.F.script (cdata_script script))

  let split_page page :
      (Html5_types.html_attrib Eliom_content.Html5.attrib list
        * (Html5_types.head_attrib Eliom_content.Html5.attrib list
            * [ Html5_types.title ] Eliom_content.Html5.elt
            * Html5_types.head_content_fun Eliom_content.Html5.elt list)
        * Html5_types.body Eliom_content.Html5.elt ) =
    match Eliom_content.Xml.content page with
      | Eliom_content.Xml.Node (_, html_attribs, [head; body]) ->
	begin match Eliom_content.Xml.content head with
	  | Eliom_content.Xml.Node (_, head_attribs, head_elts) ->
	    ( List.map Eliom_content.Html5.D.to_attrib html_attribs,
	      ( List.map Eliom_content.Html5.D.to_attrib head_attribs,
		Eliom_content.Html5.D.tot (List.hd head_elts),
		Eliom_content.Html5.D.totl (List.tl head_elts) ),
	      Eliom_content.Html5.D.tot body )
	  | _ -> assert false
	end
      | _ -> assert false

  let add_eliom_scripts ~sp page =

    lwt appl_data_script = make_eliom_appl_data_script ~sp in

    let encode_slashs = List.map (Url.encode ~plus:false) in

    let base_url = Url.make_absolute_url
      ~https:(Eliom_request_info.get_csp_ssl ())
      ~host:(Eliom_request_info.get_csp_hostname ())
      ~port:(Eliom_request_info.get_csp_server_port ())
      (String.concat "/"
         (""::encode_slashs (Eliom_request_info.get_csp_original_full_path ()))
      )
    in

    (* First we build a fake page to build the ref_tree... *)
    let (html_attribs, (head_attribs, title, head_elts), body) =
      split_page (Eliom_content.Html5.D.toelt page) in
    let head_elts =
         appl_data_script
      :: Eliom_content.Html5.F.base ~a:[a_id Eliom_common_base.base_elt_id; Eliom_content.Html5.D.a_href (Eliom_content_core.Html5.D.uri_of_string base_url)] ()
      :: ( if List.exists is_eliom_appl_script head_elts
           then head_elts
	   else ( head_elts
		  @ [application_script ()] ) )
    in
    let fake_page =
      Eliom_content.Html5.F.html ~a:html_attribs
	(Eliom_content.Html5.F.head ~a:head_attribs title head_elts)
	body in
    lwt data_script = make_eliom_data_script ~sp fake_page in

    (* Then we replace the faked data_script *)
    let head_elts =
      List.hd head_elts :: data_script :: (List.tl head_elts) in
    Lwt.return
      (Eliom_content.Html5.F.html ~a:html_attribs
	 (Eliom_content.Html5.F.head ~a:head_attribs title head_elts)
	 body )

  let remove_eliom_scripts page =
    let (html_attribs, (head_attribs, title, head_elts), body) =
      split_page (Eliom_content.Html5.D.toelt page) in
    let head_elts = List.filter (fun x -> not (is_eliom_appl_script x)) head_elts in
    Lwt.return
      (Eliom_content.Html5.F.html ~a:html_attribs
         (Eliom_content.Html5.F.head ~a:head_attribs title head_elts)
         body )

  let send_appl_content = Eliom_service.XSame_appl (Appl_params.application_name, None)

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content =


    let sp = Eliom_common.get_sp () in

    (* GRGR FIXME et si le nom de l'application diff�re ?? Il faut
       renvoyer un full_redirect... TODO *)
    if sp.Eliom_common.sp_client_appl_name <> Some Appl_params.application_name then

      Eliom_state.set_cookie
        ~cookie_level:`Client_process
        ~name:Eliom_common.appl_name_cookie_name
        ~value:Appl_params.application_name ();

    lwt page =
      match sp.Eliom_common.sp_client_appl_name, options.do_not_launch with
        | None, true -> remove_eliom_scripts content
        | _ -> add_eliom_scripts ~sp content in

    let ri = Eliom_request_info.get_ri () in
    let accept = ri.Ocsigen_extensions.ri_accept in
    lwt r = Html5_content.result_of_content ~options:accept page in

    let headers =
      match headers with
        | None -> r.Ocsigen_http_frame.res_headers
        | Some headers ->
          Http_headers.with_defaults headers r.Ocsigen_http_frame.res_headers
    in
    let headers = Http_headers.replace
      (Http_headers.name Eliom_common_base.appl_name_header_name)
      Appl_params.application_name
      headers
    in

    let rc = Eliom_request_info.get_request_cache () in
    let headers = Http_headers.replace
      (Http_headers.name Eliom_common_base.response_url_header)
      (Url.make_absolute_url
         ~https:(Eliom_request_info.get_ssl ())
         ~host:(Eliom_request_info.get_hostname ())
         ~port:(Eliom_request_info.get_server_port ())
         ("/"
          ^ try Polytables.get ~table:rc ~key:Eliom_mkreg.suffix_redir_uri_key
             (* If it is a suffix service with redirection, the uri has already been
                computed in rc *)
          with Not_found ->
            let get_params =
              match ri.Ocsigen_extensions.ri_get_params_string with
              | None -> ""
              | Some p -> "?" ^ p in
            ri.Ocsigen_extensions.ri_original_full_path_string ^ get_params))
      headers
    in

    Lwt.return
      { r with
        Ocsigen_http_frame.
        res_code    = code_of_code_option code;
        res_charset = (match charset with
          | None -> Some (Eliom_config.get_config_default_charset ())
          | _ -> charset
        );
        res_content_type =
          if Eliom_request_info.expecting_process_page ()
          then r.Ocsigen_http_frame.res_content_type
          else (match content_type with
            | None -> r.Ocsigen_http_frame.res_content_type
            | _ -> content_type);
        res_headers = headers;
      }

  end

module type ELIOM_APPL = sig
  val application_script : ?async:bool -> unit -> [> `Script ] Eliom_content.Html5.elt
  val application_name : string
  val is_initial_request : unit -> bool
  type appl
  include "sigs/eliom_reg.mli"
    subst type page    := Html5_types.html Eliom_content.Html5.elt
      and type options := appl_service_options
      and type return  := appl_service
      and type returnB := [> appl_service ]
      and type returnT := [< appl_service ]
      and type result  := appl application_content kind
  val typed_name : appl application_name
end

module App (Appl_params : APPL_PARAMS) : ELIOM_APPL = struct

  module Eliom_appl_reg_param =
    Eliom_appl_reg_make_param
      (Ocsigen_senders.Make_XML_Content(Xml)(Eliom_content.Html5.D))
      (Appl_params)

  type appl = Eliom_appl_reg_param.appl

  module Eliom_appl_registration = Eliom_mkreg.MakeRegister(Eliom_appl_reg_param)

  include Eliom_appl_registration

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  let application_name = Appl_params.application_name
  let typed_name = Appl_params.application_name
  let is_initial_request = Eliom_appl_reg_param.is_initial_request

  let application_script = Eliom_appl_reg_param.application_script

end

module type TMPL_PARAMS = sig
  type t
  val name: string
  val make_page: t -> Html5_types.html Eliom_content.Html5.elt Lwt.t
  val update: t -> unit client_value
end

module Eliom_tmpl_reg_make_param
  (Appl : ELIOM_APPL)
  (Tmpl_param : TMPL_PARAMS) = struct

  open Eliom_content.Html5
  open Html5_types

  type page = Tmpl_param.t
  type options = appl_service_options
  type return = appl_service
  type result = Appl.appl application_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XSame_appl (Appl.application_name, Some Tmpl_param.name)

  let nl_template =
    Eliom_parameter.make_non_localized_parameters
      ~prefix:"eliom" ~name:"template"
      (Eliom_parameter.string "name")

  let send ?(options = default_appl_service_options) ?charset ?code
      ?content_type ?headers content =
    match get_non_localized_get_parameters nl_template with
    | None ->
        lwt () = Eliom_reference.set request_template (Some Tmpl_param.name) in
        lwt content = Tmpl_param.make_page content in
        Result_types.cast_kind_lwt
          (Appl.send ~options ?charset ?code ?content_type ?headers content)
    | Some _ ->
        ignore (Tmpl_param.update content);
        Result_types.cast_kind_lwt (Ocaml.send ?charset ?code ?content_type ?headers ())

end

module Eliom_tmpl(Appl : ELIOM_APPL)(Tmpl_param : TMPL_PARAMS) =
  Eliom_mkreg.MakeRegister(Eliom_tmpl_reg_make_param(Appl)(Tmpl_param))

(*****************************************************************************)

(** Redirection services are like services, but send a redirection instead
 of a page.

   The HTTP/1.1 RFC says:
   If the 301 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.

   Here redirections are done towards services without parameters.
   (possibly preapplied).

 *)
module String_redir_reg_base = struct

  type page = Url.uri
  type options = [ `MovedPermanently
                 | `Found
                 | `SeeOther
                 | `NotNodifed
                 | `UseProxy
                 | `TemporaryRedirect ]
  type return = http_service
  type result = browser_content kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Found) ?charset ?code
      ?content_type ?headers content =
    let uri = content in
    let empty_result = Ocsigen_http_frame.empty_result () in
    let content_type = match content_type with
      | None -> empty_result.Ocsigen_http_frame.res_content_type
      | _ -> content_type
    in
    let headers = match headers with
      | None -> empty_result.Ocsigen_http_frame.res_headers
      | Some headers ->
        Http_headers.with_defaults
          headers empty_result.Ocsigen_http_frame.res_headers
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program
       expecting a process page,
       we do not send an HTTP redirection.
       In that case, we send a full xhr redirection.
       If the application to which belongs the destination service is the same,
       then it is ok, otherwise, there will be another redirection ...
    *)
    if not (Eliom_request_info.expecting_process_page ())
    then (* the browser did not ask application eliom data,
            we send a regular redirection *)
      let code = match code with
        | Some c -> c
        | None ->
          match options with
          | `MovedPermanently -> 301
          | `Found -> 302
          | `SeeOther -> 303
          | `NotNodifed -> 304
          | `UseProxy -> 305
          | `TemporaryRedirect -> 307
      in
      Lwt.return
        {empty_result with
          Ocsigen_http_frame.res_code= code;
          res_location = Some uri;
          res_content_type= content_type;
          res_headers= headers;
        }
    else
      Lwt.return
        {empty_result with
          Ocsigen_http_frame.res_content_type= content_type;
          res_headers=
            Http_headers.add
              (Http_headers.name Eliom_common.full_xhr_redir_header)
              uri headers
        }


end

module String_redirection = Eliom_mkreg.MakeRegister(String_redir_reg_base)

module Redir_reg_base = struct

  type ('a, 'b) page =
      (unit, unit, Eliom_service.get_service_kind,
       [ `WithoutSuffix ],
       unit, unit, Eliom_service.registrable, 'b)
        Eliom_service.service

  type options =  [ `MovedPermanently
                  | `Found
                  | `SeeOther
                  | `NotNodifed
                  | `UseProxy
                  | `TemporaryRedirect ]

  type 'a return = 'a

  type 'a result = 'a kind

  let result_of_http_result = Result_types.cast_result

  let send_appl_content = Eliom_service.XAlways
  (* actually, the service will decide itself *)

  let send ?(options = `Found) ?charset ?code
      ?content_type ?headers service =
    let uri = lazy (Eliom_content.Html5.F.make_string_uri
                      ~absolute:true ~service ()) in
    let empty_result = Ocsigen_http_frame.empty_result () in
    let content_type = match content_type with
      | None -> empty_result.Ocsigen_http_frame.res_content_type
      | _ -> content_type
    in
    let headers = match headers with
      | None -> empty_result.Ocsigen_http_frame.res_headers
      | Some headers ->
        Http_headers.with_defaults
          headers empty_result.Ocsigen_http_frame.res_headers
    in

    (* Now we decide the kind of redirection we do.
       If the request is an xhr done by a client side Eliom program
       expecting a process page,
       we do not send an HTTP redirection.
       In that case, we send:
       - a full xhr redirection if the application to which belongs
       the destination service is the same (thus it will send back tab cookies)
       - a half xhr redirection otherwise
    *)
    match Eliom_request_info.expecting_process_page (),
      Eliom_request_info.get_sp_client_appl_name () with
      (* the appl name as sent by browser *)
      | true, None (* should not happen *)
      | false, _ -> (* the browser did not ask for process data,
                       we send a regular redirection *)
        let code = match code with
          | Some c -> c
          | None ->
          match options with
          | `MovedPermanently -> 301
          | `Found -> 302
          | `SeeOther -> 303
          | `NotNodifed -> 304
          | `UseProxy -> 305
          | `TemporaryRedirect -> 307
        in
        Lwt.return
          {empty_result with
            Ocsigen_http_frame.res_code= code;
            res_location = Some (Lazy.force uri);
            res_content_type= content_type;
            res_headers= headers; }

      | true, Some anr ->
        (* the browser asked application eliom data
           for the application called anr *)
        (* If it comes from an xhr, we use answer with a special header field *)
        let headers = Http_headers.replace
          (Http_headers.name Eliom_common_base.appl_name_header_name)
          anr
          headers
        in
        match Eliom_service.get_send_appl_content service with
          (* the appl name of the destination service *)
            | Eliom_service.XSame_appl (an,_) when (an = anr) ->
            (* Same appl, we do a full xhr redirection
               (not an http redirection, because we want to
               send back tab cookies) *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      (Lazy.force uri) headers
                }

            | Eliom_service.XAlways ->
            (* It is probably an action, or a void coservice. Full xhr again *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.full_xhr_redir_header)
                      (Lazy.force uri) headers
                }

            | _ -> (* No application, or another application.
                      We ask the browser to do an HTTP redirection. *)
              Lwt.return
                {empty_result with
                  Ocsigen_http_frame.res_content_type= content_type;
                  res_headers=
                    Http_headers.add
                      (Http_headers.name Eliom_common.half_xhr_redir_header)
                      (Lazy.force uri) headers
                }


end


module Redirection = Eliom_mkreg.MakeRegister_AlphaReturn(Redir_reg_base)

(*****************************************************************************)

let set_exn_handler h =
  let sitedata = Eliom_request_info.find_sitedata "set_exn_handler" in
  Eliom_request_info.set_site_handler sitedata (Result_types.cast_function_http h)


module String = Text
