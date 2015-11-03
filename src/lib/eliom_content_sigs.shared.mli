(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015 Vasilis Papavasileiou
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

module type FORMS = sig

  include Eliom_form_sigs.S

  type (_, _, _) star

  type ('a, 'b, 'c) lazy_star =
    ?a: (('a attrib) list) ->
    ('b elt) list Eliom_lazy.request ->
    'c elt

  val lazy_form:
    ([< Html5_types.form_attrib ],
     [< Html5_types.form_content_fun ],
     [> Html5_types.form ]) lazy_star

  (** Creates an untyped form. *)
  val raw_form :
    ([< Html5_types.form_attrib ],
     [< Html5_types.form_content_fun ],
     [> Html5_types.form ]) star

  (** This is an alias to {% <<a_api|val Eliom_form_sigs.S.get_form>>
      %} to avoid the untyped [Html5_sigs.T.form]. *)
  val form :
    ?absolute:bool -> ?absolute_path:bool -> ?https:bool ->
    ?a:Html5_types.form_attrib attrib list ->
    service:
      ('get, unit,
       [< Eliom_service.get_service_kind],
       _, _,
       [< Eliom_service.suff],
       'gn, 'pn,
       [< Eliom_service.registrable],
       [< Eliom_service.non_ocaml_service ]) Eliom_service.service ->
    ?hostname:string ->
    ?port:int ->
    ?fragment:string ->
    ?keep_nl_params:[ `All | `Persistent | `None ] ->
    ?nl_params: Eliom_parameter.nl_params_set ->
    ?xhr:bool ->
    ('gn -> Html5_types.form_content elt list) ->
    [> Html5_types.form ] elt

  (** This is an alias to {% <<a_api|val
      Eliom_form_sigs.S.string_input>> %} to avoid the untyped
      [Html5_sigs.T.input]. *)
  val input :
    ?a:Html5_types.input_attrib attrib list ->
    input_type:[< Html5_types.input_type] ->
    ?name:
      [< string Eliom_parameter.setoneradio]
      Eliom_parameter.param_name ->
    ?value:string ->
    unit ->
    [> Html5_types.input ] elt

  (** This is an alias to {% <<a_api|val
      Eliom_form_sigs.S.string_select>> %} to avoid the untyped
      [Html5_sigs.T.select]. *)
  val select :
    ?a:Html5_types.select_attrib attrib list ->
    name:[ `One of string ] Eliom_parameter.param_name ->
    string select_opt ->
    string select_opt list ->
    [> Html5_types.select ] elt

end
