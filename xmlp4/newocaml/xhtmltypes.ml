(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
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
(** XHTML types with variants. (See also {!XHTML.M}) *)
(* I use these types to constraint typing with the syntax extension *)
(*
type tag = [ `Br | `Span | `Bdo | `Map | `Object | `Img | `Tt | `I | `B | `Big
           | `Small | `Em | `Strong | `Dfn | `Code | `Q | `Samp | `Kbd | `Var
           | `Cite | `Abbr | `Acronym | `Sub | `Sup | `Input | `Select
           | `Textarea | `Label | `Button | `Ins | `Del | `Script | `Noscript
           | `A | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Ul | `Ol | `Dl | `Pre
           | `Hr | `Blockquote | `Address | `P | `Div | `Fieldset | `Table
           | `Form | `Html | `Head | `Body | `Title | `Base | `Style | `Meta
           | `Link | `Li | `Dt | `Dd | `Param | `Area | `Optgroup | `Option
           | `Legend | `Caption | `Thead | `Tfoot | `Tbody | `Colgroup
           | `Col | `Tr | `Th | `Td]
*)
open XHTML.M;
type xhtml = [ = `Html ];
type form = [ = `Form ];
type a = [ = `A ];
type img = [ = `Img ];
type link = [ = `Link ];
type script = [ = `Script ];
type input = [ = `Input ];
type textarea = [ = `Textarea ];
type select = [ = `Select ];
type selectoption = [ = `Option ];
type optgroup = [ = `Optgroup ];
type button = [ = `Button ];
type pcdata = [ = `PCDATA ];
type notag;
type html_content = [ = `Body | `Head | `Frameset ];
type body_content = block;
(* [ `Address | `Blockquote | `Del | `Div | `Dl | `Fieldset
| `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins | `Noscript | `Ol
| `P | `Pre | `Script | `Table | `Ul ] *)
type div_content = [ = `PCDATA | flow ];
(* [ `A | `Abbr | `Acronym | `Address | `B | `Bdo | `Big | `Blockquote | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Div | `Dl | `Em | `Fieldset | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `I | `Img | `Input | `Ins | `Kbd | `Label | `Map | `Noscript | `Object | `Ol | `P | `PCDATA | `Pre | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Table | `Textarea | `Tt | `Ul | `Var ] *)
type object_content = [ = `PCDATA | flow | `Param ];
(* [ `A | `Abbr | `Acronym | `Address | `B | `Bdo | `Big | `Blockquote | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Div | `Dl | `Em | `Fieldset | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `I | `Img | `Input | `Ins | `Kbd | `Label | `Map | `Noscript | `Object | `Ol | `P | `Param | `PCDATA | `Pre | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Table | `Textarea | `Tt | `Ul | `Var ] *)
type fieldset_content = [ = `PCDATA | `Legend | flow ];
(* [ `A | `Abbr | `Acronym | `Address | `B | `Bdo | `Big | `Blockquote | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Div | `Dl | `Em | `Fieldset | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `I | `Img | `Input | `Ins | `Kbd | `Label | `Legend | `Map | `Noscript | `Object | `Ol | `P | `PCDATA | `Pre | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Table | `Textarea | `Tt | `Ul | `Var ] *)
type button_content = [ = `PCDATA | buttoncontent ];
(* [ `Abbr | `Acronym | `Address | `B | `Bdo | `Big | `Blockquote | `Br | `Cite | `Code | `Del | `Dfn | `Div | `Dl | `Em | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `I | `Img | `Ins | `Kbd | `Map | `Noscript | `Object | `Ol | `P | `PCDATA | `Pre | `Q | `Samp | `Script | `Small | `Span | `Strong | `Sub | `Sup | `Table | `Tt | `Ul | `Var ] *)
type head_content =
  [ = `Base | `Link | `Object | `Script | `Style | `Title | `Meta
  ];
(* [ `Base | `Link | `Object | `Script | `Style | `Title | `Meta ] *)
type form_content = [ = block_sans_form | `Fieldset ];
(* [ `Address | `Blockquote | `Del | `Div | `Dl | `Fieldset | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins | `Noscript | `Ol | `P | `Pre | `Script | `Table | `Ul ] *)
type blockquote_content = [ = `PCDATA | block ];
(* [ `Address | `Blockquote | `Del | `Div | `Dl | `Fieldset | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins | `Noscript | `Ol | `P | `Pre | `Script | `Table  | `Ul ] *)
type map_content = [ = block | `Area ];
(* [ `Address | `Area | `Blockquote | `Del | `Div | `Dl | `Fieldset | `Form | `H1 | `H2 | `H3 | `H4 | `H5 | `H6 | `Hr | `Ins | `Noscript | `Ol | `P | `Pre | `Script | `Table | `Ul ] *)
(* type inline_content =
    [ `A | `Abbr | `Acronym | `B | `Bdo | `Big | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Em | `I | `Img | `Input | `Ins | `Kbd | `Label | `Map | `Object | `PCDATA | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Textarea | `Tt | `Var ]
*)
type inlinemix = [ = `PCDATA | inline ];
(* [ `A | `Abbr | `Acronym | `B | `Bdo | `Big | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Em | `I | `Img | `Input | `Ins | `Kbd | `Label | `Map | `Object | `PCDATA | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Textarea | `Tt | `Var ] *)
type label_content = [ = `PCDATA | inline_sans_label ];
(* [ `A | `Abbr | `Acronym | `B | `Bdo | `Big | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Em | `I | `Img | `Input | `Ins | `Kbd | `Map | `Object | `PCDATA | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Textarea | `Tt | `Var | `Noscript ] *)
type a_content = [ = `PCDATA | inline_sans_a_mix ];
(* [ `Abbr | `Acronym | `B | `Bdo | `Big | `Br | `Button | `Cite | `Code | `Del | `Dfn | `Em | `I | `Img | `Input | `Ins | `Kbd | `Label | `Map | `Object | `PCDATA | `Q | `Samp | `Script | `Select | `Small | `Span | `Strong | `Sub | `Sup | `Textarea | `Tt | `Var | `Noscript ] *)
type pre_content = [ = `PCDATA | precontent ];
(* [ `A | `Abbr | `Acronym | `B | `Bdo | `Br | `Cite | `Code | `Dfn | `Em | `I | `Kbd | `Map | `PCDATA | `Q | `Samp | `Script | `Span | `Strong | `Tt | `Var ] *)
type dl_content = [ = `Dd | `Dt ];
type optgroup_content = [ = `Option ];
type colgroup_content = [ = `Col ];
type ul_content = [ = `Li ];
type select_content = [ = `Optgroup | `Option ];
type tbody_content = [ = `Tr ];
type table_content =
  [ = `Caption | `Col | `Colgroup | `Tbody | `Tfoot | `Thead | `Tr
  ];
type tr_content = [ = `Td | `Th ];
type abbr_content = inlinemix;
type acronym_content = inlinemix;
type address_content = inlinemix;
type b_content = inlinemix;
type bdo_content = inlinemix;
type big_content = inlinemix;
type caption_content = inlinemix;
type cite_content = inlinemix;
type code_content = inlinemix;
type dfn_content = inlinemix;
type dt_content = inlinemix;
type em_content = inlinemix;
type h1_content = inlinemix;
type h2_content = inlinemix;
type h3_content = inlinemix;
type h4_content = inlinemix;
type h5_content = inlinemix;
type h6_content = inlinemix;
type i_content = inlinemix;
type kbd_content = inlinemix;
type legend_content = inlinemix;
type p_content = inlinemix;
type q_content = inlinemix;
type samp_content = inlinemix;
type small_content = inlinemix;
type span_content = inlinemix;
type strong_content = inlinemix;
type sub_content = inlinemix;
type sup_content = inlinemix;
type tt_content = inlinemix;
type var_content = inlinemix;
type dd_content = div_content;
type del_content = div_content;
(* type div_content = div_content *)
type ins_content = div_content;
type li_content = div_content;
type th_content = div_content;
type td_content = div_content;
(* type tbody_content = body_content *)
type noscript_content = body_content;
type area_content = notag;
type base_content = notag;
type br_content = notag;
type col_content = notag;
type hr_content = notag;
type img_content = notag;
type input_content = notag;
type meta_content = notag;
type param_content = notag;
(*
type object_content = object_content
type fieldset_content = fieldset_content
type head_content = head_content
type form_content = form_content
type map_content = map_content
type label_content = label_content
type a_content = a_content
type pre_content = pre_content
type dl_content = dl_content
type optgroup_content = optgroup_content
type colgroup_content = colgroup_content
type ul_content = ul_content
type select_content = select_content
type table_content = table_content
type tr_content = tr_content
type button_content = button_content
type blockquote_content = blockquote_content
*)
type link_content = pcdata;
type option_content = pcdata;
type script_content = pcdata;
type style_content = pcdata;
type textarea_content = pcdata;
type title_content = pcdata;
type ol_content = ul_content;
type thead_content = tbody_content;
type tfoot_content = tbody_content;
type a_attrib =
  [ = common | `Accesskey | `Charset | `Href | `Hreflang | `Name_01_00 | `Rel
    | `Rev | `Tabindex | `Target | `Type | `Shape | `Coords
  ];
type link_attrib =
  [ = common | `Charset | `Href | `Hreflang | `Media | `Rel | `Rev | `Target
    | `Type
  ];
type script_attrib = [ = `XMLns | `Charset | `Src | `Defer | `XML_space ];
type form_attrib =
  [ = common | `Enctype | `Method | `Name_01_00 | `Target | `Accept_charset
    | `Accept
  ];
type img_attrib =
  [ = common | `Height | `Longdesc | `Name_01_00 | `Width | `Usemap
  ];
type div_attrib = common;
type input_attrib =
  [ = common | `Accesskey | `Checked | `Maxlength | `Name | `Size | `Src
    | `Tabindex | `Input_Type | `Value | `Disabled | `Readonly | `Alt
    | `Accept | `Usemap
  ];
type textarea_attrib =
  [ = common | `Accesskey | `Name | `Tabindex | `Disabled | `Readonly
  ];
type select_attrib =
  [ = common | `Multiple | `Name | `Size | `Tabindex | `Disabled
  ];
type optgroup_attrib = [ = common | `Disabled | `Label ];
type option_attrib = [ = common | `Selected | `Value | `Disabled | `Label ];
type button_attrib = [ = common | `Name | `Value | `Button_Type ];
type fieldset_attrib = common;
type label_attrib = [ = common | `Accesskey | `For ];
type legend_attrib = [ = common | `Accesskey ];

