(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_predefmod
 * Copyright (C) 2007 Vincent Balat
 * Laboratoire PPS - CNRS Université Paris Diderot
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




open Lwt
open Ocsigen_lib
open XHTML.M
open Xhtmltypes
open Ocsigen_extensions
open Eliom_sessions
open Eliom_services
open Eliom_parameters
open Eliom_mkforms
open Eliom_mkreg

open Ocsigen_http_frame
open Ocsigen_http_com

include Eliom_predefmod_client

module type ELIOMSIG = sig
  include Eliom_mkreg.ELIOMREGSIG
  include Eliom_mkforms.ELIOMFORMSIG
end

let code_of_code_option = function
  | None -> 200
  | Some c -> c

let result_of_content_subxhtml get_etag c =
  let x = Xhtmlpretty_streams.xhtml_list_stream c in
  let default_result = default_result () in
  Lwt.return
    {default_result with
       res_content_length = None;
       res_content_type = Some "text/html";
       res_etag = get_etag c;
       res_headers= Http_headers.dyn_headers;
       res_stream = (x, None)
    }


module Xhtmlreg_(Xhtml_content : Ocsigen_http_frame.HTTP_CONTENT
                         with type t = [ `Html ] XHTML.M.elt
                   and type options = XHTML.M.doctypes
                ) = struct
  open XHTML.M
  open Xhtmltypes

  type page = xhtml elt

  type options = XHTML.M.doctypes

  type return = Eliom_services.http

  module Xhtml_content = struct

    include Xhtml_content

    let add_css (a : 'a) : 'a =
      let css =
        XHTML.M.toelt
          (XHTML.M.style ~contenttype:"text/css"
             [XHTML.M.pcdata "\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n"])
      in
      let rec aux = function
        | { XML.elt = XML.Element ("head",al,el ) } as e::l ->
            { e with XML.elt = XML.Element ("head",al,css::el) }::l
        | { XML.elt = XML.BlockElement ("head",al,el) } as e::l ->
            { e with XML.elt = XML.BlockElement ("head",al,css::el) }::l
        | { XML.elt = XML.SemiBlockElement ("head",al,el) } as e::l ->
            { e with XML.elt = XML.SemiBlockElement ("head",al,css::el) }::l
        | { XML.elt = XML.Node ("head",al,el) } as e::l ->
            { e with XML.elt = XML.Node ("head",al,css::el) }::l
        | e::l -> e::(aux l)
        | [] -> []
      in
      XHTML.M.tot
        (match XHTML.M.toelt a with
           | { XML.elt = XML.Element ("html",al,el) } as e ->
               { e with XML.elt = XML.Element ("html",al,aux el) }
           | { XML.elt = XML.BlockElement ("html",al,el) } as e ->
               { e with XML.elt = XML.BlockElement ("html",al,aux el) }
           | { XML.elt = XML.SemiBlockElement ("html",al,el) } as e ->
               { e with XML.elt = XML.SemiBlockElement ("html",al,aux el) }
           | { XML.elt = XML.Node ("html",al,el) } as e ->
               { e with XML.elt = XML.Node ("html",al,aux el) }
           | e -> e)

    let get_etag ?options c = get_etag (add_css c)

    let result_of_content ?options c = result_of_content ?options (add_css c)

  end

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?(options = `XHTML_01_01) ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp content =
    Xhtml_content.result_of_content ~options content >>= fun r ->
    Lwt.return
      {r with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module Xhtmlreg = MakeRegister(Xhtmlreg_(Ocsigen_senders.Xhtml_content))
module Xhtmlcompactreg =
  MakeRegister(Xhtmlreg_(Ocsigen_senders.Xhtmlcompact_content))


module Xhtml = struct
  include Xhtmlforms
  include Xhtmlreg
end

module Xhtmlcompact' = Xhtmlcompact
module Xhtmlcompact = struct
  include Xhtmlforms
  include Xhtmlcompactreg
end


(****************************************************************************)
(****************************************************************************)
module SubXhtml = functor(T : sig type content end) ->
  (struct
(*    module Old_Cont_content =
      (* Pasted from ocsigen_senders.ml and modified *)
      struct
        type t = T.content XHTML.M.elt list

        let get_etag_aux x =
          Some (Digest.to_hex (Digest.string x))

        let get_etag c =
          let x = (Xhtmlpretty.ocsigen_xprint c) in
          get_etag_aux x

        let result_of_content c =
          let x = Xhtmlpretty.ocsigen_xprint c in
          let md5 = get_etag_aux x in
          let default_result = default_result () in
          Lwt.return
            {default_result with
             res_content_length = Some (Int64.of_int (String.length x));
             res_content_type = Some "text/html";
             res_etag = md5;
             res_headers= Http_headers.dyn_headers;
             res_stream =
             Ocsigen_stream.make
               (fun () -> Ocsigen_stream.cont x
                   (fun () -> Ocsigen_stream.empty None))
           }

      end *)

    module Cont_content =
      (* Pasted from ocsigen_senders.ml and modified *)
      struct
        type t = T.content XHTML.M.elt list

        let get_etag_aux x = None

        let get_etag ?options c = None

        let result_of_content c = result_of_content_subxhtml get_etag c

      end

    module Contreg_ = struct
      open XHTML.M
      open Xhtmltypes

      type page = T.content XHTML.M.elt list

      type options = unit

      type return = Eliom_services.http

      let pre_service ?options ~sp = Lwt.return ()

      let application_name = None

      let send ?options ?(cookies=[]) ?charset ?code 
          ?content_type ?headers ~sp content =
        Cont_content.result_of_content content >>= fun r ->
        Lwt.return
          {r with
             res_cookies= 
              Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
             res_code= code_of_code_option code;
             res_charset= (match charset with
                             | None -> Some (get_config_default_charset sp)
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

    module Contreg = MakeRegister(Contreg_)

    include Xhtmlforms
    include Contreg

  end : sig

    include ELIOMREGSIG with type page = T.content XHTML.M.elt list
                        and type options = unit
                        and type return = Eliom_services.http
    include XHTMLFORMSSIG

  end)

module Blocks = SubXhtml(struct
  type content = Xhtmltypes.body_content
end)


(****************************************************************************)
(****************************************************************************)

module Textreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = (string * string)

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?code 
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Text_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (get_config_default_charset sp)
                         | _ -> charset);
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

module Text = MakeRegister(Textreg_)

(****************************************************************************)
(****************************************************************************)

module CssTextreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Text_content.result_of_content (content, "text/css")
    >>= fun r ->
    Lwt.return
      {r with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module CssText = MakeRegister(CssTextreg_)


(****************************************************************************)
(****************************************************************************)

module HtmlTextreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?code 
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Text_content.result_of_content (content, "text/html")
    >>= fun r ->
    Lwt.return
      {r with
         res_cookies= 
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
                         | _ -> charset);
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

module HtmlTextforms_ = struct
  open XHTML.M
  open Xhtmltypes

  type form_content_elt = string
  type form_content_elt_list = string
  type uri = string
  type a_content_elt = string
  type a_content_elt_list = string
  type div_content_elt = string
  type div_content_elt_list = string

  type a_elt = string
  type a_elt_list = string
  type form_elt = string

  type textarea_elt = string
  type input_elt = string
  type select_elt = string
  type select_content_elt = string
  type select_content_elt_list = string
  type option_elt = string
  type option_elt_list = string
  type button_elt = string
  type button_content_elt = string
  type button_content_elt_list = string

  type link_elt = string
  type script_elt = string

  type pcdata_elt = string

  type a_attrib_t = string
  type form_attrib_t = string
  type input_attrib_t = string
  type textarea_attrib_t = string
  type select_attrib_t = string
  type link_attrib_t = string
  type script_attrib_t = string
  type optgroup_attrib_t = string
  type option_attrib_t = string
  type button_attrib_t = string

  type input_type_t = string
  type button_type_t = string

  let hidden = "hidden"
(*  let text = "text"
  let password = "password" *)
  let checkbox = "checkbox"
  let radio = "radio"
  let submit = "submit"
  let file = "file"
  let image = "image"

  let buttonsubmit = "submit"

  let uri_of_string x = x

  let empty_seq = ""
  let cons_form a l = a^l

  let map_option f =
    List.fold_left (fun d a -> d^(f a)) ""

  let map_optgroup f a l =
    ((f a), List.fold_left (fun d a -> d^(f a)) "" l)

  let select_content_of_option = id

  let make_pcdata = id

  let make_a ?(a="") ?href ?onclick l : a_elt =
    let a = match href with
      | None -> a
      | Some v -> " href=\""^v^"\" "^a
    in
    let a = match onclick with
      | None -> a
      | Some v -> " onclick=\""^v^"\" "^a
    in
    "<a "^a^">"^(* List.fold_left (^) "" l *) l^"</a>"

  let make_get_form ?(a="") ~action elt1 elts : form_elt =
    "<form method=\"get\" action=\""^(uri_of_string action)^"\""^a^">"^
    elt1^(*List.fold_left (^) "" elts *) elts^"</form>"

  let make_post_form ?(a="") ~action ?id ?(inline = false) elt1 elts
      : form_elt =
    let aa = "enctype=\"multipart/form-data\" "
        (* Always Multipart!!! How to test if there is a file?? *)
      ^(match id with
        None -> a
      | Some i -> " id="^i^" "^a)
    in
    "<form method=\"post\" action=\""^(uri_of_string action)^"\""^
    (if inline then "style=\"display: inline\"" else "")^aa^">"^
    elt1^(* List.fold_left (^) "" elts*) elts^"</form>"

  let make_hidden_field content =
    let content = match content with
      | None -> ""
      | Some c -> c
    in
    "<div style=\"display: none\""^content^"</div>"

  let remove_first l = "",l

  let make_input ?(a="") ?(checked=false) ~typ ?name ?src ?value () =
    let a2 = match value with
      None -> a
    | Some v -> " value="^v^" "^a
    in
    let a2 = match name with
      None -> a2
    | Some v -> " name="^v^" "^a2
    in
    let a2 = match src with
      None -> a2
    | Some v -> " src="^v^" "^a2
    in
    let a2 = if checked then " checked=\"checked\" "^a2 else a2 in
    "<input type=\""^typ^"\" "^a2^"/>"

  let make_button ?(a="") ~button_type ?name ?value c =
    let a2 = match value with
      None -> a
    | Some v -> " value="^v^" "^a
    in
    let a2 = match name with
      None -> a2
    | Some v -> " name="^v^" "^a2
    in
    "<button type=\""^button_type^"\" "^a2^">"^c^"</button>"

  let make_textarea ?(a="") ~name:name ?(value="") ~rows ~cols () =
    "<textarea name=\""^name^"\" rows=\""^(string_of_int rows)^
    "\" cols=\""^(string_of_int cols)^"\" "^a^">"^value^"</textarea>"

  let make_select ?(a="") ~multiple ~name elt elts =
    "<select "^(if multiple then "multiple=\"multiple\" " else "")^
    "name=\""^name^"\" "^a^">"^elt^elts^"</select>"

  let make_option ?(a="") ~selected ?value c =
    let a = match value with
      None -> a
    | Some v -> " value="^v^" "^a
    in
    "<option "^(if selected then "selected=\"selected\" " else "")^
    a^">"^c^"</option>"

  let make_optgroup ?(a="") ~label elt elts =
    "<optgroup label=\""^label^"\" "^
    a^">"^elt^elts^"</optgroup>"


  let make_css_link ?(a="") ~uri () =
    "<link href=\""^uri^" type=\"text/css\" rel=\"stylesheet\" "^a^"/>"

  let make_js_script ?(a="") ~uri () =
    "<script src=\""^uri^" contenttype=\"text/javascript\" "^a^"></script>"

  let register_event elt ev callback v =
    failwith "register_event not implemented for text"

end



(****************************************************************************)
(****************************************************************************)

module HtmlTextforms = MakeForms(HtmlTextforms_)
module HtmlTextreg = MakeRegister(HtmlTextreg_)

module HtmlText = struct
  include HtmlTextforms
  include HtmlTextreg
end


(****************************************************************************)
(****************************************************************************)

(** Actions are like services, but do not generate any page. The current
   page is reloaded (but if you give the optional parameter
    [~options:`NoReload] to the registration function).
 *)
module Actionreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = unit

  type options = [ `Reload | `NoReload ]

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send
      ?(options = `Reload) ?(cookies=[]) ?charset ?(code = 204)
      ?content_type ?headers ~sp () =
    let cookies_set_by_page = cookies in
    if options = `NoReload
    then
      let empty_result = Ocsigen_http_frame.empty_result () in
      Lwt.return
        {empty_result with
           res_cookies=
            Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
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
      let sitedata = Eliom_sessions.get_sitedata ~sp in
      let si = Eliom_sessions.get_si ~sp in
      let ri = Eliom_sessions.get_request ~sp in
      let all_user_cookies =
        Eliommod_cookies.add_cookie_list_to_send
          sitedata
          (Eliom_services.eccookiel_of_escookiel cookies_set_by_page)
          Ocsigen_http_frame.Cookies.empty
      in

      (match si.Eliom_common.si_nonatt_info,
         si.Eliom_common.si_state_info,
         ri.request_info.ri_method with
           | Eliom_common.RNa_no,
             (Eliom_common.RAtt_no, Eliom_common.RAtt_no), 
             Ocsigen_http_frame.Http_header.GET ->
             let empty_result = Ocsigen_http_frame.empty_result () in
             Lwt.return empty_result 
           | _ ->

               let all_cookie_info = 
                 (Eliom_sessions.esp_of_sp sp).Eliom_common.sp_cookie_info 
               in
               Eliommod_cookies.compute_new_ri_cookies
                 (Unix.time ())
                 ri.request_info.ri_sub_path
                 (Lazy.force ri.request_info.ri_cookies)
                 all_cookie_info
                 (Eliom_services.eccookiel_of_escookiel cookies_set_by_page)
               >>= fun ric ->

               Eliommod_cookies.compute_cookies_to_send
                 sitedata
                 all_cookie_info
                 all_user_cookies
               >>= fun all_new_cookies ->

               (match
                  si.Eliom_common.si_nonatt_info,
                  si.Eliom_common.si_state_info,
                  ri.request_info.ri_method
                with
                  | Eliom_common.RNa_get_ _,
                    (_, Eliom_common.RAtt_no), 
                    Ocsigen_http_frame.Http_header.GET
                  | Eliom_common.RNa_get' _,
                    (_, Eliom_common.RAtt_no), 
                    Ocsigen_http_frame.Http_header.GET
                      (* no post params, GET na coservice *)
                  | Eliom_common.RNa_no,
                    (_, Eliom_common.RAtt_no), 
                    Ocsigen_http_frame.Http_header.GET
                      (* no post params, GET attached coservice *)
                    ->
                      Polytables.set
                        ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                        Eliom_common.eliom_params_after_action
                        (si.Eliom_common.si_all_get_params,
                         si.Eliom_common.si_all_post_params, (* is [] *)
                         si.Eliom_common.si_nl_get_params,
                         si.Eliom_common.si_nl_post_params,
                         si.Eliom_common.si_all_get_but_nl)
                      ;
                      let ri =
                        {ri.request_info with
                           ri_cookies= lazy ric;
                           ri_get_params = 
                            lazy si.Eliom_common.si_other_get_params;
       (* Here we modify ri, 
          thus the request can be taken by other extensions, 
          with its new parameters *)
                        }
                      in
                      Ocsigen_extensions.serve_request 
                        ~previous_cookies:all_new_cookies ri
                        

                  | Eliom_common.RNa_post_ _, (_, _), _
                  | Eliom_common.RNa_post' _, (_, _), _ ->
                      (* POST na coservice *)
                      (* retry without POST params *)
                      
                      Polytables.set
                        ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                        Eliom_common.eliom_params_after_action
                        (si.Eliom_common.si_all_get_params,
                         si.Eliom_common.si_all_post_params,
                         si.Eliom_common.si_nl_get_params,
                         si.Eliom_common.si_nl_post_params,
                         si.Eliom_common.si_all_get_but_nl)
                      ;
                      let ri =
                        {ri.request_info with
                           ri_method = Ocsigen_http_frame.Http_header.GET;
                           ri_cookies= lazy ric;
                           ri_get_params = 
                            lazy si.Eliom_common.si_other_get_params;
                           ri_post_params = (fun _ -> Lwt.return []);
                           ri_files = (fun _ -> Lwt.return []);
                        }
                      in
                      Ocsigen_extensions.serve_request
                        ~previous_cookies:all_new_cookies ri

                  | _ ->
                        (* retry without POST params *)
(*VVV 
Warning: is it possible to have POST method but no POST parameter?
--> may loop...
(we impose GET)
*)
                      Polytables.set
                        ri.Ocsigen_extensions.request_info.Ocsigen_extensions.ri_request_cache
                        Eliom_common.eliom_params_after_action
                        (si.Eliom_common.si_all_get_params,
                         si.Eliom_common.si_all_post_params,
                         si.Eliom_common.si_nl_get_params,
                         si.Eliom_common.si_nl_post_params,
                         si.Eliom_common.si_all_get_but_nl)
                      ;
                      let ri = 
                        {ri.request_info with
                           ri_method = Ocsigen_http_frame.Http_header.GET;
                           ri_cookies= lazy ric;
                           ri_get_params = 
                            lazy si.Eliom_common.si_other_get_params;
                           ri_post_params = (fun _ -> Lwt.return []);
                           ri_files = (fun _ -> Lwt.return []);
                        }
                      in
                      Ocsigen_extensions.serve_request
                        ~previous_cookies:all_new_cookies ri)
      )

end

module Action = MakeRegister(Actionreg_)




(** Unit services are like services, do not generate any page, and do not
    reload the page. To be used carefully. Probably not usefull at all.
    (Same as {!Eliom_predefmod.Action} with [`NoReload] option).
 *)
module Unitreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = unit

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?(code = 204)
      ?content_type ?headers ~sp content =
    let empty_result = Ocsigen_http_frame.empty_result () in
    Lwt.return
      {empty_result with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
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


module Unit = MakeRegister(Unitreg_)


(** Redirection services are like services, but send a redirection instead
 of a page.

   The HTTP/1.1 RFC says:
   If the 301 status code is received in response to a request other than GET or HEAD, the user agent MUST NOT automatically redirect the request unless it can be confirmed by the user, since this might change the conditions under which the request was issued.

   Here redirections are done towards services without parameters.
   (possibly preapplied).

 *)
module String_redirreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = XHTML.M.uri

  type options = [ `Temporary | `Permanent ]

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?(options = `Permanent) ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp content =
    let empty_result = Ocsigen_http_frame.empty_result () in
    let code = match code with
    | Some c -> c
    | None ->
        if options = `Temporary
        then 307 (* Temporary move *)
        else 301 (* Moved permanently *)
    in
    Lwt.return
      {empty_result with
         res_cookies= 
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code;
         res_location = Some (XHTML.M.string_of_uri content);
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


module String_redirection = MakeRegister(String_redirreg_)




module Redirreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = 
      (unit, unit, Eliom_services.get_service_kind,
       [ `WithoutSuffix ], 
       unit, unit, Eliom_services.registrable, Eliom_services.http)
        Eliom_services.service

  type options = [ `Temporary | `Permanent ]

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?(options = `Permanent) ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp content =
    let empty_result = Ocsigen_http_frame.empty_result () in
    let uri = Xhtml.make_string_uri ~absolute:true ~sp ~service:content () in
    let code = match code with
    | Some c -> c
    | None ->
        if options = `Temporary
        then 307 (* Temporary move *)
        else 301 (* Moved permanently *)
    in
    Lwt.return
      {empty_result with
         res_cookies= 
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code;
         res_location = Some uri;
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


module Redirection = MakeRegister(Redirreg_)



(* Any is a module allowing to register services that decide themselves
   what they want to send.
 *)
module Anyreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = Ocsigen_http_frame.result

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp res =
    Lwt.return
      {res with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies
            ~oldtable:res.res_cookies
            ~sp
            cookies;
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

module Any = MakeRegister(Anyreg_)


(* Files is a module allowing to register services that send files *)
module Filesreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp filename =
    let file =
      try Ocsigen_LocalFiles.resolve (Eliom_sessions.get_request sp) filename
      with
        | Ocsigen_LocalFiles.Failed_403 (* XXXBY : maybe we should signal a true 403? *)
        | Ocsigen_LocalFiles.Failed_404
        | Ocsigen_LocalFiles.NotReadableDirectory ->
            raise Eliom_common.Eliom_404
    in
    Ocsigen_LocalFiles.content ~request:(Eliom_sessions.get_request sp) ~file
    >>= fun r ->
    Lwt.return
      { r with
          res_cookies =
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
          res_code = code_of_code_option code;
          res_charset = (match charset with
                           | None ->
                               Some (Ocsigen_charset_mime.find_charset
                                       filename(get_config_info sp).charset_assoc)
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

module Files = MakeRegister(Filesreg_)

(****************************************************************************)
(****************************************************************************)

module Streamlistreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = (((unit -> (string Ocsigen_stream.t) Lwt.t) list) *
                 string)

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp content =
    Ocsigen_senders.Streamlist_content.result_of_content content >>= fun r ->
    Lwt.return
      {r with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None ->  Some (get_config_default_charset sp)
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

module Streamlist = MakeRegister(Streamlistreg_)



(****************************************************************************)
(****************************************************************************)

module Camlreg_ = struct
  open XHTML.M
  open Xhtmltypes

  type page = string

  type options = unit

  type return = Eliom_services.http

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?cookies ?charset ?code 
      ?content_type ?headers ~sp content =
    Text.send ?options ?cookies ?charset ?code 
      ?content_type ?headers ~sp (content, "application/x-eliom")

end

module Caml = struct
  module M = MakeRegister(Camlreg_)

  type options = unit

(* the string is urlencoded because otherwise js does strange things
   with strings ... *)
  let encode_data r = Ocsigen_lib.encode ~plus:false (Marshal.to_string r [])

  let make_eh = function
    | None -> None
    | Some eh -> 
        Some (fun sp l -> 
                eh sp l >>= fun r ->
                Lwt.return (encode_data r))

  let make_service_handler f =
    fun sp g p -> 
      f sp g p >>= fun r -> 
      Lwt.return (encode_data r)

  let pre_service ?options ~sp = Lwt.return ()

  let application_name = None

  let send ?options ?cookies ?charset ?code 
      ?content_type ?headers ~sp content =
    M.send ?options ?cookies ?charset ?code 
      ?content_type ?headers ~sp (encode_data content)

  let register
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ~(service : ('get, 'post,
                   [< internal_service_kind ],
                   [< suff ], 'gn, 'pn, [ `Registrable ], 
                   'return Eliom_parameters.caml) service)
      ?(error_handler : (Eliom_sessions.server_params ->
                           (string * exn) list -> 'return Lwt.t) option)
      (f : (Eliom_sessions.server_params -> 'get -> 'post -> 'return Lwt.t)) =
    M.register
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ~service:(Eliom_services.untype_service_ service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)


  let register_for_session
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure
      ~sp
      ~service
      ?error_handler
      f =
    M.register_for_session
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure
      ~sp
      ~service:(Eliom_services.untype_service_ service)
      ?error_handler:(make_eh error_handler)
      (make_service_handler f)

  let register_new_service 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ?https
      ~path
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_service 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?sp
                                      ?https
                                      ~path
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_coservice 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_coservice 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_coservice' 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_coservice' 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_coservice_for_session 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure
      ~sp
      ?name
      ?csrf_safe
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_coservice_for_session 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure
                                      ~sp
                                      ?name
                                      ?csrf_safe
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_coservice_for_session' 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure
      ~sp
      ?name
      ?csrf_safe
      ?max_use
      ?timeout
      ?https
      ~get_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_coservice_for_session' 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure
                                      ~sp
                                      ?name
                                      ?csrf_safe
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~get_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_post_service 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_post_service 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?sp
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_post_coservice 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_post_coservice 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_post_coservice' 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?sp
      ?name
      ?csrf_safe
      ?csrf_session_name
      ?csrf_secure_session
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_post_coservice' 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?sp
                                      ?name
                                      ?csrf_safe
                                      ?csrf_session_name
                                      ?csrf_secure_session
                                      ?max_use
                                      ?timeout
                                      ?keep_get_na_params
                                      ?https
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f))

  let register_new_post_coservice_for_session 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure
      ~sp
      ?name
      ?csrf_safe
      ?max_use
      ?timeout
      ?https
      ~fallback
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_post_coservice_for_session 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure
                                      ~sp
                                      ?name
                                      ?csrf_safe
                                      ?max_use
                                      ?timeout
                                      ?https
                                      ~fallback:(Eliom_services.untype_service_ fallback)
                                      ~post_params
                                      ?error_handler:(make_eh error_handler)
                                      (make_service_handler f)) 

  let register_new_post_coservice_for_session' 
      ?options
      ?cookies
      ?charset
      ?code
      ?content_type
      ?headers
      ?session_name
      ?secure
      ~sp
      ?name
      ?csrf_safe
      ?max_use
      ?timeout
      ?keep_get_na_params
      ?https
      ~post_params
      ?error_handler
      f =
    Eliom_services.untype_service_ (M.register_new_post_coservice_for_session' 
                                      ?options
                                      ?cookies
                                      ?charset
                                      ?code
                                      ?content_type
                                      ?headers
                                      ?session_name
                                      ?secure
                                      ~sp
                                      ?name
                                      ?csrf_safe
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

type appl_service_params =
    {
      ap_doctype: XHTML.M.doctypes;
      ap_title: string;
      ap_container : 'a.
        ((([< XHTML.M.common ] as 'a) XHTML.M.attrib list) option *
           (Xhtmltypes.body_content elt -> Xhtmltypes.body_content elt list))
        option;
      ap_body_attributes : 
        'a. (([< XHTML.M.common ] as 'a) XHTML.M.attrib list) option;
      ap_headers : [ `Meta | `Link | `Style | `Object | `Script ] elt list
    }

module type APPL_PARAMS = sig
     val application_name : string
     val params : appl_service_params
end

let default_appl_params =
  { ap_doctype = `XHTML_01_01;
    ap_title = "Eliom application";
    ap_container = None;
    ap_body_attributes = None;
    ap_headers = [];
  }

module Eliom_appl_reg_
  (Xhtml_content : Ocsigen_http_frame.HTTP_CONTENT
   with type t = [ `Html ] XHTML.M.elt
   and type options = XHTML.M.doctypes
  )
  (Appl_params : APPL_PARAMS) = struct
  open XHTML.M
  open Xhtmltypes

  type page = body_content elt list

  type options = bool

  type return = Eliom_services.appl_service

  let create_page ~sp params do_not_launch_application content = 
    let body, container_node = match params.ap_container with
      | None -> let b = XHTML.M.body ?a:params.ap_body_attributes content in
        (b, (XHTML.M.toelt b))
      | Some (a, container) ->
          let d = XHTML.M.div ?a content in
          (XHTML.M.body
             ?a:params.ap_body_attributes 
             (container d),
           (XHTML.M.toelt d))
    in
    XHTML.M.html
      (XHTML.M.head (XHTML.M.title (XHTML.M.pcdata params.ap_title)) 
         (
           XHTML.M.style ~contenttype:"text/css"
             [XHTML.M.pcdata "\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n"]::

             (* This will do a redirection if there is a #! in the URL *)
             XHTML.M.script ~contenttype:"text/javascript"
             (cdata_script
                ("// Redirect if the URL contains #! while loading the page
function redir () {
  var str_url = window.location.toString() ;
  try{
    var match = str_url.match(\"(.*)/[^#/?]*(\\\\?.*)?#!((https?://)?(.*))$\");
          //but what if there's a # the search ?
    if(match) {
      if(match[4]) { //absolute
        window.location = match[3];
        alert(\"Absolute redirection to \"+window.location);
      }
      else { //relative
        window.location = match[1] + \"/\" + match[3] ;
        alert(\"Relative redirection to \"+match[1] + \" / \" + match[3]);
      }
    }
  } catch(e) {} ;
};
redir ();"))::

             if not do_not_launch_application
             then
               (* O'Browser: *)
               XHTML.M.script ~a:[a_src (Xhtml.make_uri 
                                           (Eliom_services.static_dir ~sp)
                                           sp
                                           ["vm.js"])]
                 ~contenttype:"text/javascript" (pcdata "")::
                 
                 (* JS part of Eliom client for O'Browser: *)
                 XHTML.M.script ~a:[a_src (Xhtml.make_uri 
                                             (Eliom_services.static_dir ~sp)
                                             sp
                                             ["eliom_obrowser.js"])]
                 ~contenttype:"text/javascript" (pcdata "")::
                 
                 
                 XHTML.M.script ~contenttype:"text/javascript"
                 (cdata_script
                    (* eliom_id_tree is some information for relinking the
                       nodes on client side.
                       Relinking is done in Eliom_obrowser_client.
                    *)
                    ("window.onload = function () { \n"
                     ^ "  eliom_id_tree = input_val (" ^ 
                     (Eliom_client_types.jsmarshal
                        (XML.make_ref_tree (XHTML.M.toelt body))) ^ "); \n"
                     
                     ^ "  eliom_global_data = input_val (" ^ 
                     (Eliom_client_types.jsmarshal
                        (Eliom_client.get_global_eliom_appl_data_ ~sp)
                     ) ^ "); \n"
                     
                     ^ "  container_node = input_val (" ^ 
                     let reqnum = Eliom_sessions.get_request_id ~sp in
                     (Eliom_client_types.jsmarshal
                        (Eliom_client_types.to_data_key_ 
                           (reqnum, XML.ref_node container_node))
                     ) ^ "); \n"
                       
                     ^ "  appl_name = \"" ^ 
                       (Appl_params.application_name
                       ) ^ "\"; \n"
                       
                     ^ "  appl_instance_id = \"" ^ 
                       (match Eliom_sessions.get_application_instance ~sp with
                          | Some s -> s
                          | None -> "<error: application instance id not created>"
                       ) ^ "\"; \n"
                       
                     (* The main client side program: *)
                     ^ "  main_vm = exec_caml (\"" ^ 
                       Appl_params.application_name ^ ".uue\") ; \n"
                     ^ " }"))::
                 params.ap_headers
             else params.ap_headers

         ))
      body

  let pre_service ?(options = false) ~sp =
    (* If we launch a new application, we must set the application name
       and create an application instance id *)
    if options ||
      Eliom_sessions.get_content_only ~sp (* the application already exists *)
    then Lwt.return ()
    else begin
      let rc = Eliom_sessions.get_request_cache ~sp in
      Polytables.set ~table:rc ~key:Eliom_parameters.appl_name_key
        ~value:(Some Appl_params.application_name);
      Polytables.set ~table:rc ~key:Eliom_parameters.appl_instance_key 
        ~value:(Some (Eliommod_cookies.make_new_cookie_value ()));
      Lwt.return ()
    end

  let application_name = Some Appl_params.application_name

  let send ?(options = false) ?(cookies=[]) ?charset ?code
      ?content_type ?headers ~sp content =
    let content_only = Eliom_sessions.get_content_only ~sp in
    (if content_only
(*VVV do not send container! *)
     then 
(*VVV Here we do not send a stream *)
       Caml.send ~sp ((XML.make_ref_tree_list (XHTML.M.toeltl content)),
                      (Eliom_client.get_global_eliom_appl_data_ ~sp),
(*VVV Use another serialization format than XML for the page? *)
                      Xhtmlcompact'.xhtml_list_print content)
     else 
(*VVV for now not possible to give other params for one page *)
       let page = create_page ~sp Appl_params.params options content in
       let options = Appl_params.params.ap_doctype in
       Xhtml_content.result_of_content ~options page)
    >>= fun r ->
    Lwt.return
      {r with
         res_cookies=
          Eliom_services.cookie_table_of_eliom_cookies ~sp cookies;
         res_code= code_of_code_option code;
         res_charset= (match charset with
                         | None -> Some (get_config_default_charset sp)
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

module Eliom_appl (Appl_params : APPL_PARAMS) = struct
  include Xhtmlforms
  include MakeRegister(Eliom_appl_reg_
                         (Ocsigen_senders.Xhtmlcompact_content)
                         (Appl_params))

  (** Unique identifier for this application.
      It is the application name.
      Warning: do not mix up with the "application instance id",
      that is unique for each instance of the application.
  *)
  let application_name = Appl_params.application_name
end

