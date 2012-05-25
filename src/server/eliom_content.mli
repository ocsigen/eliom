(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2012 Vincent Balat, Benedikt Becker
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


(** Abstract signature for links and forms creation functions. For
    concrete instance see {!Html5}, {!Xhtml} or {!Html_text}. *)
module type Forms = "sigs/eliom_forms.mli"

module Xml : module type of Eliom_content_core.Xml
    with type uri = Eliom_content_core.Xml.uri
    and type attrib = Eliom_content_core.Xml.attrib
    and type elt = Eliom_content_core.Xml.elt
    and type event_handler = Eliom_content_core.Xml.event_handler
    and type event_handler_table = Eliom_content_core.Xml.event_handler_table
    and type -'a caml_event_handler = 'a Eliom_content_core.Xml.caml_event_handler

module Svg : module type of Eliom_content_core.Svg
    with type uri = Eliom_content_core.Svg.uri
    and type 'a attrib = 'a Eliom_content_core.Svg.attrib
    and type +'a elt = 'a Eliom_content_core.Svg.elt

(** Building and printing valid (X)HTML5 tree. *)
module Html5 : sig

  (** See the Eliom manual for more information on{% <<a_manual
      chapter="client" fragment="unique"| dom semantics vs. functional
      semantics>> %} for HTML5 tree manipulated by client/server
      application. *)

  type +'a elt = 'a Eliom_content_core.Html5.elt
  type +'a attrib = 'a Eliom_content_core.Html5.attrib
  type uri = Eliom_content_core.Html5.uri

  module F : sig
    include module type of Eliom_content_core.Html5.F
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a elt
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include "sigs/eliom_html5_forms.mli"
  end

  module D : sig
    include module type of Eliom_content_core.Html5.D
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a elt
                   and type 'a attrib = 'a attrib
                   and type uri = uri
    include "sigs/eliom_html5_forms.mli"
  end

  module Id : module type of Eliom_content_core.Html5.Id

  module Printer : module type of Eliom_content_core.Html5.Printer

end

module Xhtml : sig
  module F : sig
    include module type of Eliom_content_core.Xhtml.F
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a Eliom_content_core.Xhtml.F.elt
                   and type 'a attrib = 'a Eliom_content_core.Xhtml.F.attrib
                   and type uri = Eliom_content_core.Xhtml.F.uri
    include "sigs/eliom_xhtml_forms.mli"
  end
  module F_01_00 : sig
    include module type of Eliom_content_core.Xhtml.F_01_00
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a Eliom_content_core.Xhtml.F_01_00.elt
                   and type 'a attrib = 'a Eliom_content_core.Xhtml.F_01_00.attrib
                   and type uri = Eliom_content_core.Xhtml.F_01_00.uri
    include "sigs/eliom_xhtml_forms.mli"
  end
  module F_01_01 : sig
    include module type of Eliom_content_core.Xhtml.F_01_01
                   with type Xml.uri = Xml.uri
                   and type Xml.event_handler = Xml.event_handler
                   and type Xml.attrib = Xml.attrib
                   and type Xml.elt = Xml.elt
                   with type +'a elt = 'a Eliom_content_core.Xhtml.F_01_01.elt
                   and type 'a attrib = 'a Eliom_content_core.Xhtml.F_01_01.attrib
                   and type uri = Eliom_content_core.Xhtml.F_01_01.uri
    include "sigs/eliom_xhtml_forms.mli"
  end
  module Printer : module type of Eliom_content_core.Xhtml.Printer
  module Printer_01_00 : module type of Eliom_content_core.Xhtml.Printer_01_00
  module Printer_01_01 : module type of Eliom_content_core.Xhtml.Printer_01_01
end

module Html_text : sig
  include "sigs/eliom_forms.mli"
    subst type uri := string
    and type pcdata_elt := string

    and type form_elt := string
    and type form_content_elt := string
    and type form_content_elt_list := string
    and type form_attrib_t := string

    and type 'a a_elt := string
    and type 'a a_content_elt := string
    and type 'a a_content_elt_list := string
    and type a_attrib_t := string

    and type link_elt := string
    and type link_attrib_t := string

    and type script_elt := string
    and type script_attrib_t := string

    and type textarea_elt := string
    and type textarea_attrib_t := string

    and type input_elt := string
    and type input_attrib_t := string

    and type select_elt := string
    and type select_attrib_t := string

    and type button_elt := string
    and type button_content_elt := string
    and type button_content_elt_list := string
    and type button_attrib_t := string

    and type optgroup_attrib_t := string
    and type option_attrib_t := string

    and type input_type_t := string
    and type raw_input_type_t := string
    and type button_type_t := string
    and type for_attrib := string
end
