include "eliom_forms.mli"
         subst type uri := XML.uri
           and type pcdata_elt := XHTML_types.pcdata XHTML.F.elt

           and type form_elt := [> XHTML_types.form ] XHTML.F.elt
           and type form_content_elt := XHTML_types.form_content XHTML.F.elt
           and type form_content_elt_list := XHTML_types.form_content XHTML.F.elt list
           and type form_attrib_t := XHTML_types.form_attrib XHTML.F.attrib list

           and type 'a a_elt := [> XHTML_types.a ] XHTML.F.elt
           and type 'a a_content_elt := XHTML_types.a_content XHTML.F.elt
           and type 'a a_content_elt_list := XHTML_types.a_content XHTML.F.elt list
           and type a_attrib_t := XHTML_types.a_attrib XHTML.F.attrib list

           and type link_elt := [> XHTML_types.link ] XHTML.F.elt
           and type link_attrib_t := XHTML_types.link_attrib XHTML.F.attrib list

           and type script_elt := [> XHTML_types.script ] XHTML.F.elt
           and type script_attrib_t := XHTML_types.script_attrib XHTML.F.attrib list

           and type textarea_elt := [> XHTML_types.textarea ] XHTML.F.elt
           and type textarea_attrib_t := XHTML_types.textarea_attrib XHTML.F.attrib list

           and type input_elt := [> XHTML_types.input ] XHTML.F.elt
           and type input_attrib_t := XHTML_types.input_attrib XHTML.F.attrib list

           and type select_elt := [> XHTML_types.select ] XHTML.F.elt
           and type select_attrib_t := XHTML_types.select_attrib XHTML.F.attrib list

           and type button_elt := [> XHTML_types.button ] XHTML.F.elt
           and type button_content_elt := XHTML_types.button_content XHTML.F.elt
           and type button_content_elt_list := XHTML_types.button_content XHTML.F.elt list
           and type button_attrib_t := XHTML_types.button_attrib XHTML.F.attrib list

           and type optgroup_attrib_t := [ XHTML_types.common | `Disabled ] XHTML.F.attrib list
           and type option_attrib_t := XHTML_types.option_attrib XHTML.F.attrib list

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

         and type for_attrib := [> `For ] XHTML.F.attrib
