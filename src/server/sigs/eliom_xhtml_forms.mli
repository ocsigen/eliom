include "eliom_forms.mli"
         subst type uri := Eliom_content_core.Xml.uri
           and type pcdata_elt := Xhtml_types.pcdata Eliom_content_core.Xhtml.F.elt

           and type form_elt := [> Xhtml_types.form ] Eliom_content_core.Xhtml.F.elt
           and type form_content_elt := Xhtml_types.form_content Eliom_content_core.Xhtml.F.elt
           and type form_content_elt_list := Xhtml_types.form_content Eliom_content_core.Xhtml.F.elt list
           and type form_attrib_t := Xhtml_types.form_attrib Eliom_content_core.Xhtml.F.attrib list

           and type 'a a_elt := [> Xhtml_types.a ] Eliom_content_core.Xhtml.F.elt
           and type 'a a_content_elt := Xhtml_types.a_content Eliom_content_core.Xhtml.F.elt
           and type 'a a_content_elt_list := Xhtml_types.a_content Eliom_content_core.Xhtml.F.elt list
           and type a_attrib_t := Xhtml_types.a_attrib Eliom_content_core.Xhtml.F.attrib list

           and type link_elt := [> Xhtml_types.link ] Eliom_content_core.Xhtml.F.elt
           and type link_attrib_t := Xhtml_types.link_attrib Eliom_content_core.Xhtml.F.attrib list

           and type script_elt := [> Xhtml_types.script ] Eliom_content_core.Xhtml.F.elt
           and type script_attrib_t := Xhtml_types.script_attrib Eliom_content_core.Xhtml.F.attrib list

           and type textarea_elt := [> Xhtml_types.textarea ] Eliom_content_core.Xhtml.F.elt
           and type textarea_attrib_t := Xhtml_types.textarea_attrib Eliom_content_core.Xhtml.F.attrib list

           and type input_elt := [> Xhtml_types.input ] Eliom_content_core.Xhtml.F.elt
           and type input_attrib_t := Xhtml_types.input_attrib Eliom_content_core.Xhtml.F.attrib list

           and type select_elt := [> Xhtml_types.select ] Eliom_content_core.Xhtml.F.elt
           and type select_attrib_t := Xhtml_types.select_attrib Eliom_content_core.Xhtml.F.attrib list

           and type button_elt := [> Xhtml_types.button ] Eliom_content_core.Xhtml.F.elt
           and type button_content_elt := Xhtml_types.button_content Eliom_content_core.Xhtml.F.elt
           and type button_content_elt_list := Xhtml_types.button_content Eliom_content_core.Xhtml.F.elt list
           and type button_attrib_t := Xhtml_types.button_attrib Eliom_content_core.Xhtml.F.attrib list

           and type optgroup_attrib_t := [ Xhtml_types.common | `Disabled ] Eliom_content_core.Xhtml.F.attrib list
           and type option_attrib_t := Xhtml_types.option_attrib Eliom_content_core.Xhtml.F.attrib list

           and type input_type_t :=
	     [< `Hidden
	     | `Password
	     | `Submit
	     | `Text
	     ]

           and type raw_input_type_t :=
	     [< `Hidden
	     | `Password
	     | `Submit
	     | `Text
	     | `Button
	     | `Reset ]

           and type button_type_t :=
	     [< `Button
	     | `Reset
	     | `Submit
	     ]

         and type for_attrib := [> `For ] Eliom_content_core.Xhtml.F.attrib
