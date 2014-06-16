include "sigs/eliom_forms.mli"
  subst type uri := Eliom_content.Xml.uri
    and type pcdata_elt := Html5_types.pcdata Eliom_content.Html5.elt

    and type form_elt := [> Html5_types.form ] Eliom_content.Html5.elt
    and type form_content_elt := Html5_types.form_content Eliom_content.Html5.elt
    and type form_content_elt_list := Html5_types.form_content Eliom_content.Html5.elt list
    and type form_attrib_t := Html5_types.form_attrib Eliom_content.Html5.attrib list

    and type 'a a_elt := [> 'a Html5_types.a ] Eliom_content.Html5.elt
    and type 'a a_content_elt := 'a Eliom_content.Html5.elt
    and type 'a a_content_elt_list := 'a Eliom_content.Html5.elt list
    and type a_attrib_t := Html5_types.a_attrib Eliom_content.Html5.attrib list

    and type link_elt := [> Html5_types.link ] Eliom_content.Html5.elt
    and type link_attrib_t := Html5_types.link_attrib Eliom_content.Html5.attrib list

    and type script_elt := [> Html5_types.script ] Eliom_content.Html5.elt
    and type script_attrib_t := Html5_types.script_attrib Eliom_content.Html5.attrib list

    and type textarea_elt := [> Html5_types.textarea ] Eliom_content.Html5.elt
    and type textarea_attrib_t := Html5_types.textarea_attrib Eliom_content.Html5.attrib list

    and type input_elt := [> Html5_types.input ] Eliom_content.Html5.elt
    and type input_attrib_t := Html5_types.input_attrib Eliom_content.Html5.attrib list

    and type select_elt := [> Html5_types.select ] Eliom_content.Html5.elt
    and type select_attrib_t := Html5_types.select_attrib Eliom_content.Html5.attrib list

    and type button_elt := [> Html5_types.button ] Eliom_content.Html5.elt
    and type button_content_elt := Html5_types.button_content Eliom_content.Html5.elt
    and type button_content_elt_list := Html5_types.button_content Eliom_content.Html5.elt list
    and type button_attrib_t := Html5_types.button_attrib Eliom_content.Html5.attrib list

    and type optgroup_attrib_t := [ Html5_types.common | `Disabled ] Eliom_content.Html5.attrib list
    and type option_attrib_t := Html5_types.option_attrib Eliom_content.Html5.attrib list

    and type input_type_t :=
        [<
	| `Url
	| `Tel
	| `Text
	| `Time
	| `Search
	| `Password
	| `Checkbox
	| `Range
	| `Radio
	| `Submit
	| `Reset
	| `Number
	| `Hidden
	| `Month
	| `Week
	| `File
	| `Email
	| `Image
	| `Datetime_local
	| `Datetime
	| `Date
	| `Color
	| `Button]

    and type button_type_t :=
      [< `Button
      | `Reset
      | `Submit
      ]

    and type for_attrib := [> `For ] Eliom_content.Html5.attrib
