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
open XHTML5.M;
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
type body_content = flow5;
type div_content = flow5;
type object_content = phrasing;
type fieldset_content = flow5;
type button_content = phrasing_without_interactive;
type head_content = [ = metadata | `Title ];
type form_content = flow5_without_form;
type blockquote_content = flow5;
type map_content = [ = phrasing | `Area ];
type label_content = text;
type a_content = phrasing_without_interactive;
type pre_content = phrasing;
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
type abbr_content = phrasing;
type acronym_content = phrasing;
type address_content = phrasing;
type b_content = phrasing;
type bdo_content = phrasing;
type big_content = phrasing;
type caption_content = phrasing;
type cite_content = phrasing;
type code_content = phrasing;
type dfn_content = phrasing;
type dt_content = phrasing;
type em_content = phrasing;
type h1_content = phrasing;
type h2_content = phrasing;
type h3_content = phrasing;
type h4_content = phrasing;
type h5_content = phrasing;
type h6_content = phrasing;
type i_content = phrasing;
type kbd_content = phrasing;
type legend_content = phrasing;
type p_content = phrasing;
type q_content = phrasing;
type samp_content = phrasing;
type small_content = phrasing;
type span_content = phrasing;
type strong_content = phrasing;
type sub_content = phrasing;
type sup_content = phrasing;
type tt_content = phrasing;
type var_content = phrasing;
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
  [ = common | `Href | `Hreflang | `Media | `Rel | `Target | `Mime_type ];
type link_attrib =
  [ = common | `Href | `Hreflang | `Media
  | `Rel | `Sizes | `Mime_type ];
type script_attrib = 
  [ = common | `Async | `Charset | `Src | `Defer |`Mime_type ];
type form_attrib =
  [ = common |`Accept_charset | `Action | `Enctype | `Method | `Name | `Target | `Autocomplete | `Novalidate ];
type img_attrib =
  [ = common | `Height | `Longdesc | `Name_01_00 | `Width | `Usemap
  ];
type div_attrib = common;
type input_attrib =
  [ = common | `Accept | `Alt | `Autocomplete
  | `Autofocus | `Checked | `Disabled | `Form
  | `Formation | `Formenctype | `Formmethod
  | `Formnovalidate | `Formtarget | `Height | `List
  | `Input_Max | `Maxlength | `Input_Min | `Multiple | `Name
  | `Pattern | `Placeholder | `ReadOnly | `Required|`Size
  | `Src | `Step | `Input_Type | `Value | `Width ];
type textarea_attrib =
  [ = common | `Autofocus | `Disabled | `Form | `Maxlength
  | `Name | `Placeholder| `Readonly| `Required | `Wrap | `Rows | `Cols];
type select_attrib =
  [ = common | `Multiple | `Name | `Size | `Tabindex | `Disabled
  ];
type optgroup_attrib = [ = common | `Disabled | `Label ];
type option_attrib = [ = common | `Selected | `Text_Value | `Disabled | `Label ];
type button_attrib = [ = common | `Autofocus | `Disabled | `Form
                     | `Formaction | `Formenctype | `Formmethod
                     | `Formnovalidate | `Formtarget | `Name
                     | `Text_Value | `Button_Type ];
type fieldset_attrib = common;
type label_attrib = [ = common | `Accesskey | `For ];
type legend_attrib = [ = common | `Accesskey ];

