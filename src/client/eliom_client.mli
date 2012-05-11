(* Ocsigen
 * http://www.ocsigen.org
 * Module eliom_client.ml
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

(** Call server side services and change the current page. *)

open Eliom_lib

(** Call a server side service and change the current page.
    If the service belongs to the same application,
    the client side program is not stopped, and only
    the content (not the container) is reloaded. *)
val change_page :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e,
           [< Eliom_services.registrable ], Eliom_output.appl_service)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit Lwt.t

(** Call a server side service that return an OCaml value. *)
val call_caml_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e,
           [< Eliom_services.registrable ], 'return Eliom_parameters.caml)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool -> 'a -> 'b -> 'return Lwt.t


(** Stop current program and load a new page.  Note that for string arguments,
    sole line feed or sole carriage return characters are substituted by the
    string ["\r\n"]. *)
val exit_to :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e,
           [< Eliom_services.registrable ], [< Eliom_output.non_caml_service ])
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool -> 'a -> 'b -> unit

(** Loads an Eliom service in a window (cf. Javascript's [window.open]). *)
val window_open :
  window_name:Js.js_string Js.t ->
  ?window_features:Js.js_string Js.t ->
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, unit,
           [< Eliom_services.get_service_kind ],
           [< `WithSuffix | `WithoutSuffix ], _, unit,
           [< Eliom_services.registrable ], _)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool -> 'a -> Dom_html.window Js.t

(** (low level) Call a server side service and return the content
    of the resulting HTTP frame as a string. *)
val call_service :
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:('a, 'b,
           [< Eliom_services.service_kind ],
           [< `WithSuffix | `WithoutSuffix ], 'd, 'e,
           [< Eliom_services.registrable ], 'return)
          Eliom_services.service ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Eliom_parameters.nl_params_set ->
  ?keep_get_na_params:bool -> 'a -> 'b -> string Lwt.t

(** wait for the loading phase to terminate *)
val wait_load_end : unit -> unit Lwt.t

(** true if the function is executed inside the loading phase *)
val in_onload : unit -> bool

(** register a function to be called on page change *)
val on_unload : (unit -> unit) -> unit

module Html5 : sig
  open Eliom_content_core

  val of_element : 'a HTML5.elt -> Dom_html.element Js.t
  val of_heading : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t

  val of_pcdata : [> `Pcdata] HTML5.elt -> Dom.text Js.t

  val of_abbr : [> `Abbr] HTML5.elt -> Dom_html.element Js.t
  val of_acronym : [> `Acronym] HTML5.elt -> Dom_html.element Js.t
  val of_address : [> `Address] HTML5.elt -> Dom_html.element Js.t
  val of_applet : [> `Applet] HTML5.elt -> Dom_html.element Js.t
  val of_article : [> `Article] HTML5.elt -> Dom_html.element Js.t
  val of_aside : [> `Aside] HTML5.elt -> Dom_html.element Js.t
  val of_audio : [> `Audio] HTML5.elt -> Dom_html.element Js.t
  val of_b : [> `B] HTML5.elt -> Dom_html.element Js.t
  val of_basefont : [> `basefont] HTML5.elt -> Dom_html.element Js.t
  val of_bdi : [> `Bdi] HTML5.elt -> Dom_html.element Js.t
  val of_bdo : [> `Bdo] HTML5.elt -> Dom_html.element Js.t
  val of_big : [> `Big] HTML5.elt -> Dom_html.element Js.t
  val of_center : [> `Center] HTML5.elt -> Dom_html.element Js.t
  val of_cite : [> `Cite] HTML5.elt -> Dom_html.element Js.t
  val of_code : [> `Code] HTML5.elt -> Dom_html.element Js.t
  val of_colgroup : [> `Colgroup] HTML5.elt -> Dom_html.element Js.t
  val of_command : [> `Command] HTML5.elt -> Dom_html.element Js.t
  val of_datalist : [> `Datalist] HTML5.elt -> Dom_html.element Js.t
  val of_dd : [> `Dd] HTML5.elt -> Dom_html.element Js.t
  val of_del : [> `Del] HTML5.elt -> Dom_html.element Js.t
  val of_details : [> `Details] HTML5.elt -> Dom_html.element Js.t
  val of_dfn : [> `Dfn] HTML5.elt -> Dom_html.element Js.t
  val of_dir : [> `Dir] HTML5.elt -> Dom_html.element Js.t
  val of_dt : [> `Dt] HTML5.elt -> Dom_html.element Js.t
  val of_em : [> `Em] HTML5.elt -> Dom_html.element Js.t
  val of_embed : [> `Embed] HTML5.elt -> Dom_html.element Js.t
  val of_figcaption : [> `Figcaption] HTML5.elt -> Dom_html.element Js.t
  val of_figure : [> `Figure] HTML5.elt -> Dom_html.element Js.t
  val of_font : [> `Font] HTML5.elt -> Dom_html.element Js.t
  val of_footer : [> `Footer] HTML5.elt -> Dom_html.element Js.t
  val of_frame : [> `Frame] HTML5.elt -> Dom_html.element Js.t
  val of_frameset : [> `Frameset] HTML5.elt -> Dom_html.element Js.t
  val of_h1 : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t
  val of_h2 : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t
  val of_h3 : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t
  val of_h4 : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t
  val of_h5 : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t
  val of_h6 : HTML5_types.heading HTML5.elt -> Dom_html.headingElement Js.t
  val of_header : [> `Header] HTML5.elt -> Dom_html.element Js.t
  val of_hgroup : [> `Hgroup] HTML5.elt -> Dom_html.element Js.t
  val of_i : [> `I] HTML5.elt -> Dom_html.element Js.t
  val of_ins : [> `Ins] HTML5.elt -> Dom_html.element Js.t
  val of_keygen : [> `Keygen] HTML5.elt -> Dom_html.element Js.t
  val of_kbd : [> `Kbd] HTML5.elt -> Dom_html.element Js.t
  val of_mark : [> `Mark] HTML5.elt -> Dom_html.element Js.t
  val of_menu : [> `Menu] HTML5.elt -> Dom_html.element Js.t
  val of_meter : [> `Meter] HTML5.elt -> Dom_html.element Js.t
  val of_nav : [> `Nav] HTML5.elt -> Dom_html.element Js.t
  val of_noframes : [> `Noframes] HTML5.elt -> Dom_html.element Js.t
  val of_noscript : [> `Noscript] HTML5.elt -> Dom_html.element Js.t
  val of_output : [> `Output] HTML5.elt -> Dom_html.element Js.t
  val of_progress : [> `Progress] HTML5.elt -> Dom_html.element Js.t
  val of_q : [> `Q] HTML5.elt -> Dom_html.element Js.t
  val of_rp : [> `Rp] HTML5.elt -> Dom_html.element Js.t
  val of_rt : [> `Rt] HTML5.elt -> Dom_html.element Js.t
  val of_ruby : [> `Ruby] HTML5.elt -> Dom_html.element Js.t
  val of_s : [> `S] HTML5.elt -> Dom_html.element Js.t
  val of_samp : [> `Samp] HTML5.elt -> Dom_html.element Js.t
  val of_section : [> `Section] HTML5.elt -> Dom_html.element Js.t
  val of_small : [> `Small] HTML5.elt -> Dom_html.element Js.t
  val of_source : [> `Source] HTML5.elt -> Dom_html.element Js.t
  val of_span : [> `Span] HTML5.elt -> Dom_html.element Js.t
  val of_strike : [> `Strike] HTML5.elt -> Dom_html.element Js.t
  val of_strong : [> `Strong] HTML5.elt -> Dom_html.element Js.t
  val of_sub : [> `Sub] HTML5.elt -> Dom_html.element Js.t
  val of_summary : [> `Summary] HTML5.elt -> Dom_html.element Js.t
  val of_sup : [> `Sup] HTML5.elt -> Dom_html.element Js.t
  val of_th : [> `Th] HTML5.elt -> Dom_html.element Js.t
  val of_time : [> `Time] HTML5.elt -> Dom_html.element Js.t
  val of_track : [> `Track] HTML5.elt -> Dom_html.element Js.t
  val of_tt : [> `Tt] HTML5.elt -> Dom_html.element Js.t
  val of_u : [> `U] HTML5.elt -> Dom_html.element Js.t
  val of_var : [> `Var] HTML5.elt -> Dom_html.element Js.t
  val of_video : [> `Video] HTML5.elt -> Dom_html.element Js.t
  val of_wbr : [> `Wbr] HTML5.elt -> Dom_html.element Js.t

  val of_html : HTML5_types.html HTML5.elt -> Dom_html.htmlElement Js.t
  val of_head : HTML5_types.head HTML5.elt -> Dom_html.headElement Js.t
  val of_link : HTML5_types.link HTML5.elt -> Dom_html.linkElement Js.t
  val of_title : HTML5_types.title HTML5.elt -> Dom_html.titleElement Js.t
  val of_meta : HTML5_types.meta HTML5.elt -> Dom_html.metaElement Js.t
  val of_base : HTML5_types.base HTML5.elt -> Dom_html.baseElement Js.t
  val of_style : HTML5_types.style HTML5.elt -> Dom_html.styleElement Js.t
  val of_body : HTML5_types.body HTML5.elt -> Dom_html.bodyElement Js.t
  val of_form : HTML5_types.form HTML5.elt -> Dom_html.formElement Js.t
  val of_optgroup : HTML5_types.optgroup HTML5.elt -> Dom_html.optGroupElement Js.t
  val of_option : HTML5_types.selectoption HTML5.elt -> Dom_html.optionElement Js.t
  val of_select : HTML5_types.select HTML5.elt -> Dom_html.selectElement Js.t
  val of_input : HTML5_types.input HTML5.elt -> Dom_html.inputElement Js.t
  val of_textarea : HTML5_types.textarea HTML5.elt -> Dom_html.textAreaElement Js.t
  val of_button : HTML5_types.button HTML5.elt -> Dom_html.buttonElement Js.t
  val of_label : HTML5_types.label HTML5.elt -> Dom_html.labelElement Js.t
  val of_fieldset : HTML5_types.fieldset HTML5.elt -> Dom_html.fieldSetElement Js.t
  val of_legend : HTML5_types.legend HTML5.elt -> Dom_html.legendElement Js.t
  val of_ul : HTML5_types.ul HTML5.elt -> Dom_html.uListElement Js.t
  val of_ol : HTML5_types.ol HTML5.elt -> Dom_html.oListElement Js.t
  val of_dl : [`Dl] HTML5.elt -> Dom_html.dListElement Js.t
  val of_li : HTML5_types.li HTML5.elt -> Dom_html.liElement Js.t
  val of_div : HTML5_types.div HTML5.elt -> Dom_html.divElement Js.t
  val of_p : HTML5_types.p HTML5.elt -> Dom_html.paragraphElement Js.t
  val of_blockquote : HTML5_types.blockquote HTML5.elt -> Dom_html.quoteElement Js.t
  val of_pre : HTML5_types.pre HTML5.elt -> Dom_html.preElement Js.t
  val of_br : HTML5_types.br HTML5.elt -> Dom_html.brElement Js.t
  val of_hr : HTML5_types.hr HTML5.elt -> Dom_html.hrElement Js.t
  val of_a : 'a HTML5_types.a HTML5.elt -> Dom_html.anchorElement Js.t
  val of_img : [`Img] HTML5.elt -> Dom_html.imageElement Js.t
  val of_object : 'a HTML5_types.object_ HTML5.elt -> Dom_html.objectElement Js.t
  val of_param : HTML5_types.param HTML5.elt -> Dom_html.paramElement Js.t
  val of_area : HTML5_types.area HTML5.elt -> Dom_html.areaElement Js.t
  val of_map : 'a HTML5_types.map HTML5.elt -> Dom_html.mapElement Js.t
  val of_script : HTML5_types.script HTML5.elt -> Dom_html.scriptElement Js.t
  val of_td : [ HTML5_types.td | HTML5_types.td ] HTML5.elt -> Dom_html.tableCellElement Js.t
  val of_tr : HTML5_types.tr HTML5.elt -> Dom_html.tableRowElement Js.t
  val of_col : HTML5_types.col HTML5.elt -> Dom_html.tableColElement Js.t
  val of_tfoot : HTML5_types.tfoot HTML5.elt -> Dom_html.tableSectionElement Js.t
  val of_thead : HTML5_types.thead HTML5.elt -> Dom_html.tableSectionElement Js.t
  val of_tbody : HTML5_types.tbody HTML5.elt -> Dom_html.tableSectionElement Js.t
  val of_caption : HTML5_types.caption HTML5.elt -> Dom_html.tableCaptionElement Js.t
  val of_table : HTML5_types.table HTML5.elt -> Dom_html.tableElement Js.t
  val of_canvas : 'a HTML5_types.canvas HTML5.elt -> Dom_html.canvasElement Js.t
  val of_iframe : HTML5_types.iframe HTML5.elt -> Dom_html.iFrameElement Js.t

end

(**/**)

val relink_request_nodes : Dom_html.htmlElement Js.t -> unit
val reset_request_node : unit -> unit

val load_eliom_data :
  Eliom_types.eliom_js_page_data ->
  Dom_html.htmlElement Js.t -> (Dom_html.event Js.t -> bool) list

val register_closure: int64 -> ('a -> Dom_html.event Js.t -> unit) -> unit

val getElementById : string -> Dom.node Js.t
