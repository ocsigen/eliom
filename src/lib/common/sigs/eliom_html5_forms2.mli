include "sigs/eliom_forms.mli"
  subst type uri := Xml.uri
    and type pcdata_elt := Html5_types.pcdata elt

    and type form_elt := [> Html5_types.form ] elt
    and type form_content_elt := Html5_types.form_content elt
    and type form_content_elt_list := Html5_types.form_content elt list
    and type form_attrib_t := Html5_types.form_attrib attrib list

    and type 'a a_elt := [> 'a Html5_types.a ] elt
    and type 'a a_content_elt := 'a elt
    and type 'a a_content_elt_list := 'a elt list
    and type a_attrib_t := Html5_types.a_attrib attrib list

    and type link_elt := [> Html5_types.link ] elt
    and type link_attrib_t := Html5_types.link_attrib attrib list

    and type script_elt := [> Html5_types.script ] elt
    and type script_attrib_t := Html5_types.script_attrib attrib list

    and type textarea_elt := [> Html5_types.textarea ] elt
    and type textarea_attrib_t := Html5_types.textarea_attrib attrib list

    and type input_elt := [> Html5_types.input ] elt
    and type input_attrib_t := Html5_types.input_attrib attrib list

    and type select_elt := [> Html5_types.select ] elt
    and type select_attrib_t := Html5_types.select_attrib attrib list

    and type button_elt := [> Html5_types.button ] elt
    and type button_content_elt := Html5_types.button_content elt
    and type button_content_elt_list := Html5_types.button_content elt list
    and type button_attrib_t := Html5_types.button_attrib attrib list

    and type optgroup_attrib_t := [ Html5_types.common | `Disabled ] attrib list
    and type option_attrib_t := Html5_types.option_attrib attrib list

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

    and type for_attrib := [> `For ] attrib
