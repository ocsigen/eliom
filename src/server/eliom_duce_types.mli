
open Xhtml_types_duce

type uri = string
type pcdata_elt = {{ [ PCDATA ] }}

type form_elt = form
type form_content_elt = form_content
type form_content_elt_list = {{ [ form_content* ] }}
type form_attrib_t =
    {{ attrs ++ { accept-charset=?String accept=?String
           onreset=?String onsubmit=?String enctype=?String } }}

type 'a a_elt = a
type 'a a_elt_list = {{ [ a* ] }}
type 'a a_content_elt = a_content
type 'a a_content_elt_list = {{ [ a_content* ] }}
type a_attrib_t = a_attrs

type link_elt = link
type link_attrib_t = link_attrs

type script_elt = script
type script_attrib_t = {{ id ++ { defer=?"defer" src=?String charset=?String } }}
    (* {{ script_attrs -. type }} *)

type textarea_elt = textarea
type textarea_attrib_t = {{ attrs ++ focus ++
			      { onchange=?String
				  onselect=?String
				  readonly=?"readonly"
				  disabled=?"disabled"
				  name=?String } }}

type input_elt = input
type input_attrib_t = input_attrs

type select_elt = select
type select_content_elt = select_content
type select_content_elt_list = {{ [ select_content* ] }}
type select_attrib_t = select_attrs

type button_elt = button
type button_content_elt = button_content
type button_content_elt_list = {{ [ button_content* ] }}
type button_attrib_t = button_attrs

type option_elt = option
type option_elt_list = {{ [ option* ] }}
type optgroup_attrib_t = {{ attrs ++ { disabled=?"disabled" } }}
type option_attrib_t = option_attrs

type input_type_t = input_type_values
type raw_input_type_t = input_type_values
type button_type_t = button_type_values

type for_attrib = {{ { for=String } }}

  (* type div_content_elt = div_content *)
  (* type div_content_elt_list = flows *)

type html = Xhtml_types_duce.html
type anyxml = Ocamlduce.Load.anyxml
type blocks = Xhtml_duce.M.elt list

