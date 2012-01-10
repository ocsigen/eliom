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

open Eliom_pervasives

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

(** Call a server side service that return a Caml value. *)
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


(** Stop current program and load a new page. *)
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

(** (low level) Change the URL, without doing a request.
    As browsers do not not allow to change the URL (for security reasons),
    we write the new URL in the fragment part of the URL.
    A script must do the redirection if there is something in the fragment.
    Usually this function is only for internal use.
*)
val change_url :
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
  ?keep_get_na_params:bool -> 'a -> 'b -> unit

(** wait for the loading phase to terminate *)
val wait_load_end : unit -> unit Lwt.t

(** true if the function is executed inside the loading phase *)
val in_onload : unit -> bool

(** register a function to be called on page change *)
val on_unload : (unit -> unit) -> unit

module Html5 : sig

  val of_element : 'a HTML5.M.elt -> Dom_html.element Js.t

  val of_html : HTML5_types.html HTML5.M.elt -> Dom_html.htmlElement Js.t
  val of_head : HTML5_types.head HTML5.M.elt -> Dom_html.headElement Js.t
  val of_link : HTML5_types.link HTML5.M.elt -> Dom_html.linkElement Js.t
  val of_title : HTML5_types.title HTML5.M.elt -> Dom_html.titleElement Js.t
  val of_meta : HTML5_types.meta HTML5.M.elt -> Dom_html.metaElement Js.t
  val of_base : HTML5_types.base HTML5.M.elt -> Dom_html.baseElement Js.t
  val of_style : HTML5_types.style HTML5.M.elt -> Dom_html.styleElement Js.t
  val of_body : HTML5_types.body HTML5.M.elt -> Dom_html.bodyElement Js.t
  val of_form : HTML5_types.form HTML5.M.elt -> Dom_html.formElement Js.t
  val of_optgroup : HTML5_types.optgroup HTML5.M.elt -> Dom_html.optGroupElement Js.t
  val of_option : HTML5_types.selectoption HTML5.M.elt -> Dom_html.optionElement Js.t
  val of_select : HTML5_types.select HTML5.M.elt -> Dom_html.selectElement Js.t
  val of_input : HTML5_types.input HTML5.M.elt -> Dom_html.inputElement Js.t
  val of_textarea : HTML5_types.textarea HTML5.M.elt -> Dom_html.textAreaElement Js.t
  val of_button : HTML5_types.button HTML5.M.elt -> Dom_html.buttonElement Js.t
  val of_label : HTML5_types.label HTML5.M.elt -> Dom_html.labelElement Js.t
  val of_fieldset : HTML5_types.fieldset HTML5.M.elt -> Dom_html.fieldSetElement Js.t
  val of_legend : HTML5_types.legend HTML5.M.elt -> Dom_html.legendElement Js.t
  val of_ul : HTML5_types.ul HTML5.M.elt -> Dom_html.uListElement Js.t
  val of_ol : HTML5_types.ol HTML5.M.elt -> Dom_html.oListElement Js.t
  val of_dl : [`Dl] HTML5.M.elt -> Dom_html.dListElement Js.t
  val of_li : HTML5_types.li HTML5.M.elt -> Dom_html.liElement Js.t
  val of_div : HTML5_types.div HTML5.M.elt -> Dom_html.divElement Js.t
  val of_p : HTML5_types.p HTML5.M.elt -> Dom_html.paragraphElement Js.t
  val of_heading : HTML5_types.heading HTML5.M.elt -> Dom_html.headingElement Js.t
  val of_blockquote : HTML5_types.blockquote HTML5.M.elt -> Dom_html.quoteElement Js.t
  val of_pre : HTML5_types.pre HTML5.M.elt -> Dom_html.preElement Js.t
  val of_br : HTML5_types.br HTML5.M.elt -> Dom_html.brElement Js.t
  val of_hr : HTML5_types.hr HTML5.M.elt -> Dom_html.hrElement Js.t
  val of_a : 'a HTML5_types.a HTML5.M.elt -> Dom_html.anchorElement Js.t
  val of_img : [`Img] HTML5.M.elt -> Dom_html.imageElement Js.t
  val of_object : 'a HTML5_types.object_ HTML5.M.elt -> Dom_html.objectElement Js.t
  val of_param : HTML5_types.param HTML5.M.elt -> Dom_html.paramElement Js.t
  val of_area : HTML5_types.area HTML5.M.elt -> Dom_html.areaElement Js.t
  val of_map : 'a HTML5_types.map HTML5.M.elt -> Dom_html.mapElement Js.t
  val of_script : HTML5_types.script HTML5.M.elt -> Dom_html.scriptElement Js.t
  val of_td : [ HTML5_types.td | HTML5_types.td ] HTML5.M.elt -> Dom_html.tableCellElement Js.t
  val of_tr : HTML5_types.tr HTML5.M.elt -> Dom_html.tableRowElement Js.t
  val of_col : HTML5_types.col HTML5.M.elt -> Dom_html.tableColElement Js.t
  val of_tfoot : HTML5_types.tfoot HTML5.M.elt -> Dom_html.tableSectionElement Js.t
  val of_thead : HTML5_types.thead HTML5.M.elt -> Dom_html.tableSectionElement Js.t
  val of_tbody : HTML5_types.tbody HTML5.M.elt -> Dom_html.tableSectionElement Js.t
  val of_caption : HTML5_types.caption HTML5.M.elt -> Dom_html.tableCaptionElement Js.t
  val of_table : HTML5_types.table HTML5.M.elt -> Dom_html.tableElement Js.t
  val of_canvas : 'a HTML5_types.canvas HTML5.M.elt -> Dom_html.canvasElement Js.t
  val of_iframe : HTML5_types.iframe HTML5.M.elt -> Dom_html.iFrameElement Js.t

end

(**/**)

val change_page_uri :
  ?cookies_info:bool * string list ->
  ?get_params:(string * string) list -> string -> unit Lwt.t

val change_page_get_form :
  ?cookies_info:bool * string list ->
  Dom_html.formElement Js.t -> string -> unit Lwt.t

val change_page_post_form :
  ?cookies_info:bool * string list ->
  Dom_html.formElement Js.t -> string -> unit Lwt.t

val load_eliom_data :
  Eliom_types.eliom_js_page_data ->
  Dom_html.htmlElement Js.t -> (Dom_html.event Js.t -> bool) list

val register_closure: int64 -> ('a -> Dom_html.event Js.t -> unit) -> unit

