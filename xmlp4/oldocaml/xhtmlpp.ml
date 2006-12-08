(* Ocsigen
 * xhtmlpp.ml Copyright (C) 2005 Vincent Balat and Julien Mineraud
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

type attr = [`Id | `Class | `Style | `Title | `Lang | `Xmllang | `Dir
            | `Onclick | `Ondblclick | `Onmousedown | `Onmouseup
            | `Onmouseover | `Onmousemove | `Onmouseout | `Onkeypress
            | `Onkeydown | `Onkeyup | `Accesskey | `Tabindex | `Onfocus
            | `Onblur | `Xmlns | `Profile | `Httpequiv | `Name | `Content
            | `Scheme | `Charset | `Hreflang | `Type | `Media | `Rel
            | `Rev | `Xmlspace | `Src | `Defer | `Onload | `Onunload
            | `Cite | `Datetime | `Shape | `Coords | `Declare | `Classid
            | `Codebase | `Data | `Codetype | `Archive | `Standby | `Href
            | `Height | `Width | `Usemap | `Tabindex | `Value | `Valuetype
            | `Alt | `Longdesc | `Ismap | `Nohref | `Action | `Method
            | `Enctype | `Onsubmit | `Onreset | `Accept | `Acceptcharset
            | `For | `Checked | `Disabled | `Readonly | `Size | `Maxlength
            | `Onselect | `Onchange | `Multiple | `Label | `Rows | `Cols
            | `Summary | `Border | `Frame | `Rules | `Cellspacing | `Selected
            | `Cellpadding | `Span | `Align | `Char | `Charoff | `Valign
            | `Abbr | `Axis | `Headers | `Scope | `Rowspan | `Colspan]

let attrlist =
  [("id", `Id);
   ("class", `Class);
   ("style", `Style);
   ("title", `Title);
   ("lang", `Lang);
   ("xml:lang", `Xmllang);
   ("dir", `Dir);
   ("onclick", `Onclick);
   ("ondblclick", `Ondblclick);
   ("onmousedown", `Onmousedown);
   ("onmouseup", `Onmouseup);
   ("onmousemove", `Onmousemove);
   ("onmouseout", `Onmouseout);
   ("onmkeypress", `Onkeypress);
   ("onmkeydown", `Onkeydown);
   ("onmkeyup", `Onkeyup);
   ("accesskey", `Accesskey);
   ("tabindex", `Tabindex);
   ("onfocus", `Onfocus);
   ("onblur", `Onblur);
   ("onblur", `Onblur);
   ("xmlns", `Xmlns);
   ("profile", `Profile);
   ("http-equiv", `Httpequiv);
   ("name", `Name);
   ("content", `Content);
   ("scheme", `Scheme);
   ("charset", `Charset);
   ("href", `Href);
   ("hreflang", `Hreflang);
   ("type", `Type);
   ("media", `Media);
   ("rel", `Rel);
   ("rev", `Rev);
   ("xml:space", `Xmlspace);
   ("src", `Src);
   ("defer", `Defer);
   ("onload", `Onload);
   ("onunload", `Onunload);
   ("cite", `Cite);
   ("datetime", `Datetime);
   ("shape", `Shape);
   ("coords", `Coords);
   ("declare", `Declare);
   ("classid", `Classid);
   ("codebase", `Codebase);
   ("data", `Data);
   ("codetype", `Codetype);
   ("archive", `Archive);
   ("standby", `Standby);
   ("height", `Height);
   ("width", `Width);
   ("usemap", `Usemap);
   ("tabindex", `Tabindex);
   ("value", `Value);
   ("valuetype", `Valuetype);
   ("alt", `Alt);
   ("longdesc", `Longdesc);
   ("ismap", `Ismap);
   ("nohref", `Nohref);
   ("action", `Action);
   ("method", `Method);
   ("enctype", `Enctype);
   ("onsubmit", `Onsubmit);
   ("onreset", `Onreset);
   ("accept", `Accept);
   ("accept-charset", `Acceptcharset);
   ("for", `For);
   ("checked", `Checked);
   ("disabled", `Disabled);
   ("readonly", `Readonly);
   ("size", `Size);
   ("maxlength", `Maxlength);
   ("onselect", `Onselect);
   ("onchange", `Onchange);
   ("multiple", `Multiple);
   ("label", `Label);
   ("selected", `Selected);
   ("rows", `Rows);
   ("cols", `Cols);
   ("align", `Align);
   ("char", `Char);
   ("charoff", `Charoff);
   ("valign", `Valign);
   ("summary", `Summary);
   ("border", `Border);
   ("frame", `Frame);
   ("rules", `Rules);
   ("cellspacing", `Cellspacing);
   ("cellpadding", `Cellpadding);
   ("span", `Span);
   ("abbr", `Abbr);
   ("axis", `Axis);
   ("headers", `Headers);
   ("scope", `Scope);
   ("rowspan", `Rowspan);
   ("colspan", `Colspan)]

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

let taglist =
  [("br", `Br);
   ("span", `Span);
   ("bdo", `Bdo);
   ("map", `Map);
   ("object", `Object);
   ("img", `Img);
   ("tt", `Tt);
   ("i", `I);
   ("b", `B);
   ("big", `Big);
   ("small", `Small);
   ("em", `Em);
   ("strong", `Strong);
   ("dfn", `Dfn);
   ("code", `Code);
   ("q", `Q);
   ("samp", `Samp);
   ("kbd", `Kbd);
   ("var", `Var);
   ("cite", `Cite);
   ("abbr", `Abbr);
   ("acronym", `Acronym);
   ("sub", `Sub);
   ("sup", `Sup);
   ("input", `Input);
   ("select", `Select);
   ("textarea", `Textarea);
   ("label", `Label);
   ("button", `Button);
   ("ins", `Ins);
   ("del", `Del);
   ("script", `Script);
   ("noscript", `Noscript);
   ("a", `A);
   ("h1", `H1);
   ("h2", `H2);
   ("h3", `H3);
   ("h4", `H4);
   ("h5", `H5);
   ("h6", `H6);
   ("ul", `Ul);
   ("ol", `Ol);
   ("dl", `Dl);
   ("pre", `Pre);
   ("hr", `Hr);
   ("blockquote", `Blockquote);
   ("address", `Address);
   ("p", `P);
   ("div", `Div);
   ("fieldset", `Fieldset);
   ("table", `Table);
   ("form", `Form);
   ("html", `Html);
   ("head", `Head);
   ("body", `Body);
   ("title", `Title);
   ("base", `Base);
   ("style", `Style);
   ("meta", `Meta);
   ("link", `Link);
   ("li", `Li);
   ("dt", `Dt);
   ("dd", `Dd);
   ("dd", `Dd);
   ("param", `Param);
   ("area", `Area);
   ("optgroup", `Optgroup);
   ("option", `Option);
   ("legend", `Legend);
   ("caption", `Caption);
   ("thead", `Thead);
   ("tfoot", `Tfoot);
   ("tbody", `Tbody);
   ("colgroup", `Colgroup);
   ("col", `Col);
   ("tr", `Tr);
   ("th", `Th);
   ("th", `Td)]

type closed_tag = [ `Br | `Base | `Meta | `Link  | `Hr | `Param | `Img | `Area | `Input  | `Col ]

type xhnotag

type xhpcdata = [ `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *) ]

type xhalltags = 
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Acronym of (attr * string) list * xhinlinecont list
  | `Address of (attr * string) list * xhinlinecont list
  | `Area of (attr * string) list * xhnotag list
  | `B of (attr * string) list * xhinlinecont list
  | `Base of (attr * string) list * xhnotag list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Body of (attr * string) list * xhbodycont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Caption of (attr * string) list * xhinlinecont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Col of (attr * string) list * xhnotag list
  | `Colgroup of (attr * string) list * xhcolgroupcont list
  | `Dd of (attr * string) list * xhdivcont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Dt of (attr * string) list * xhinlinecont list
  | `Em of (attr * string) list * xhinlinecont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `Html of (attr * string) list * xhtmlcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Head of (attr * string) list * xhheadcont list
  | `Hr of (attr * string) list * xhnotag list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Label of (attr * string) list * xhlabelcont list
  | `Legend of (attr * string) list * xhinlinecont list
  | `Li of (attr * string) list * xhdivcont list
  | `Link of (attr * string) list * xhpcdata list
  | `Map of (attr * string) list * xhmapcont list
  | `Meta of (attr * string) list * xhnotag list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Object of (attr * string) list * xhobjectcont list
  | `Ol of (attr * string) list * xhulcont list
  | `Optgroup of (attr * string) list * xhoptgroupcont list
  | `Option of (attr * string) list * xhpcdata list
  | `P of (attr * string) list * xhinlinecont list
  | `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)
  | `Param of (attr * string) list * xhnotag list
  | `Pre of (attr * string) list * xhprecont list
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Style of (attr * string) list * xhpcdata list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Table of (attr * string) list * xhtablecont list
  | `Tbody of (attr * string) list * xhtbodycont list
  | `Td of (attr * string) list * xhdivcont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tfoot of (attr * string) list * xhtbodycont list
  | `Th of (attr * string) list * xhdivcont list
  | `Thead of (attr * string) list * xhtbodycont list
  | `Title of (attr * string) list * xhpcdata list
  | `Tr of (attr * string) list * xhtrcont list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Ul of (attr * string) list * xhulcont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhalltagsl = xhalltags list

and xhtmlcont =
    [ `Body of (attr * string) list * xhbodycont list
  | `Comment of string (* A special one for comments *)
  | `Whitespace of string
  | `Head of (attr * string) list * xhheadcont list ]

and xhbodycont =
    [ `Address of (attr * string) list * xhinlinecont list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Comment of string (* A special one for comments *)
  | `Del of (attr * string) list * xhdivcont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `Pre of (attr * string) list * xhprecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Table of (attr * string) list * xhtablecont list
  | `Whitespace of string
  | `Ul of (attr * string) list * xhulcont list ]

and xhdivcont = 
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Acronym of (attr * string) list * xhinlinecont list
  | `Address of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Em of (attr * string) list * xhinlinecont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Label of (attr * string) list * xhlabelcont list
  | `Map of (attr * string) list * xhmapcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Object of (attr * string) list * xhobjectcont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)
  | `Pre of (attr * string) list * xhprecont list
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Table of (attr * string) list * xhtablecont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Ul of (attr * string) list * xhulcont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhobjectcont = 
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Acronym of (attr * string) list * xhinlinecont list
  | `Address of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Em of (attr * string) list * xhinlinecont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Label of (attr * string) list * xhlabelcont list
  | `Map of (attr * string) list * xhmapcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Object of (attr * string) list * xhobjectcont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `Param of (attr * string) list * xhnotag list
  | `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)
  | `Pre of (attr * string) list * xhprecont list
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Table of (attr * string) list * xhtablecont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Ul of (attr * string) list * xhulcont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhfieldsetcont = 
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Acronym of (attr * string) list * xhinlinecont list
  | `Address of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Em of (attr * string) list * xhinlinecont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Label of (attr * string) list * xhlabelcont list
  | `Legend of (attr * string) list * xhinlinecont list
  | `Map of (attr * string) list * xhmapcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Object of (attr * string) list * xhobjectcont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)
  | `Pre of (attr * string) list * xhprecont list
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Table of (attr * string) list * xhtablecont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Ul of (attr * string) list * xhulcont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhbuttoncont = 
    [ `Abbr of (attr * string) list * xhinlinecont list
  | `Acronym of (attr * string) list * xhinlinecont list
  | `Address of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Br of (attr * string) list * xhnotag list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Em of (attr * string) list * xhinlinecont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Map of (attr * string) list * xhmapcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Object of (attr * string) list * xhobjectcont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)
  | `Pre of (attr * string) list * xhprecont list
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Table of (attr * string) list * xhtablecont list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Ul of (attr * string) list * xhulcont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhheadcont =
    [ `Base of (attr * string) list * xhnotag list
  | `Link of (attr * string) list * xhpcdata list
  | `Comment of string (* A special one for comments *)
  | `Whitespace of string
  | `Object of (attr * string) list * xhobjectcont list
  | `Script of (attr * string) list * xhpcdata list
  | `Style of (attr * string) list * xhpcdata list
  | `Title of (attr * string) list * xhpcdata list ]

and xhformcont = 
    [ `Address of (attr * string) list * xhinlinecont list
  | `Comment of string (* A special one for comments *)
  | `Whitespace of string
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `Pre of (attr * string) list * xhprecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Table of (attr * string) list * xhtablecont list
  | `Ul of (attr * string) list * xhulcont list ]

and xhblockquotecont = 
    [ `Address of (attr * string) list * xhinlinecont list
  | `Comment of string (* A special one for comments *)
  | `Whitespace of string
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `Pre of (attr * string) list * xhprecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Table of (attr * string) list * xhtablecont list
  | `Ul of (attr * string) list * xhulcont list ]

and xhmapcont =
    [ `Address of (attr * string) list * xhinlinecont list
  | `Area of (attr * string) list * xhnotag list
  | `Blockquote of (attr * string) list * xhblockquotecont list
  | `Comment of string (* A special one for comments *)
  | `Whitespace of string
  | `Del of (attr * string) list * xhdivcont list
  | `Div of (attr * string) list * xhdivcont list
  | `Dl of (attr * string) list * xhdlcont list
  | `Fieldset of (attr * string) list * xhfieldsetcont list
  | `Form of (attr * string) list * xhformcont list
  | `H1 of (attr * string) list * xhinlinecont list
  | `H2 of (attr * string) list * xhinlinecont list
  | `H3 of (attr * string) list * xhinlinecont list
  | `H4 of (attr * string) list * xhinlinecont list
  | `H5 of (attr * string) list * xhinlinecont list
  | `H6 of (attr * string) list * xhinlinecont list
  | `Hr of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Noscript of (attr * string) list * xhbodycont list
  | `Ol of (attr * string) list * xhulcont list
  | `P of (attr * string) list * xhinlinecont list
  | `Pre of (attr * string) list * xhprecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Table of (attr * string) list * xhtablecont list
  | `Ul of (attr * string) list * xhulcont list ]

and xhinlinecont =
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Comment of string (* A special one for comments *)
  | `Acronym of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Em of (attr * string) list * xhinlinecont list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Label of (attr * string) list * xhlabelcont list
  | `Map of (attr * string) list * xhmapcont list
  | `Object of (attr * string) list * xhobjectcont list
  | `PCData of string
  | `Whitespace of string
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhlabelcont =
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Comment of string (* A special one for comments *)
  | `Acronym of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Em of (attr * string) list * xhinlinecont list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Map of (attr * string) list * xhmapcont list
  | `Object of (attr * string) list * xhobjectcont list
  | `PCData of string
  | `Whitespace of string
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhacont =
    [ `Abbr of (attr * string) list * xhinlinecont list
  | `Comment of string (* A special one for comments *)
  | `Acronym of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Big of (attr * string) list * xhinlinecont list
  | `Br of (attr * string) list * xhnotag list
  | `Button of (attr * string) list * xhbuttoncont list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Del of (attr * string) list * xhdivcont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Em of (attr * string) list * xhinlinecont list
  | `I of (attr * string) list * xhinlinecont list
  | `Img of (attr * string) list * xhnotag list
  | `Input of (attr * string) list * xhnotag list
  | `Ins of (attr * string) list * xhdivcont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Label of (attr * string) list * xhlabelcont list
  | `Map of (attr * string) list * xhmapcont list
  | `Object of (attr * string) list * xhobjectcont list
  | `PCData of string
  | `Whitespace of string
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Select of (attr * string) list * xhselectcont list
  | `Small of (attr * string) list * xhinlinecont list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Sub of (attr * string) list * xhinlinecont list
  | `Sup of (attr * string) list * xhinlinecont list
  | `Textarea of (attr * string) list * xhpcdata list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhprecont =
    [ `A of (attr * string) list * xhacont list
  | `Abbr of (attr * string) list * xhinlinecont list
  | `Acronym of (attr * string) list * xhinlinecont list
  | `B of (attr * string) list * xhinlinecont list
  | `Bdo of (attr * string) list * xhinlinecont list
  | `Br of (attr * string) list * xhnotag list
  | `Cite of (attr * string) list * xhinlinecont list
  | `Code of (attr * string) list * xhinlinecont list
  | `Dfn of (attr * string) list * xhinlinecont list
  | `Em of (attr * string) list * xhinlinecont list
  | `I of (attr * string) list * xhinlinecont list
  | `Kbd of (attr * string) list * xhinlinecont list
  | `Map of (attr * string) list * xhmapcont list
  | `PCData of string
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)
  | `Q of (attr * string) list * xhinlinecont list
  | `Samp of (attr * string) list * xhinlinecont list
  | `Script of (attr * string) list * xhpcdata list
  | `Span of (attr * string) list * xhinlinecont list
  | `Strong of (attr * string) list * xhinlinecont list
  | `Tt of (attr * string) list * xhinlinecont list
  | `Var of (attr * string) list * xhinlinecont list ]

and xhdlcont =
    [ `Dd of (attr * string) list * xhdivcont list
  | `Comment of string (* A special one for comments *)
  | `Whitespace of string
  | `Dt of (attr * string) list * xhinlinecont list ]

and xhtml = 
  [ `Html of (attr * string) list * xhtmlcont list 
  | `Whitespace of string
  | `Comment of string (* A special one for comments *) ]

and xhoptgroupcont = 
    [ `Option of (attr * string) list * xhpcdata list 
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)]

and xhcolgroupcont = 
    [ `Col of (attr * string) list * xhnotag list 
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)]

and xhulcont =
    [ `Li of (attr * string) list * xhdivcont list 
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)]

and xhselectcont =
    [ `Optgroup of (attr * string) list * xhoptgroupcont list
  | `Option of (attr * string) list * xhpcdata list
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)]

and xhtbodycont =
    [ `Tr of (attr * string) list * xhtrcont list
  | `Whitespace of string
  | `Comment of string (* A special one for comments *)]

and xhtablecont =
    [ `Caption of (attr * string) list * xhinlinecont list
  | `Whitespace of string
  | `Col of (attr * string) list * xhnotag list
  | `Colgroup of (attr * string) list * xhcolgroupcont list
  | `Comment of string (* A special one for comments *)
  | `Tbody of (attr * string) list * xhtbodycont list
  | `Tfoot of (attr * string) list * xhtbodycont list
  | `Thead of (attr * string) list * xhtbodycont list
  | `Tr of (attr * string) list * xhtrcont list ]

and xhtrcont =
    [ `Td of (attr * string) list * xhdivcont list
  | `Th of (attr * string) list * xhdivcont list
  | `Whitespace of string
  | `Comment of string (* A special one for comments *) ]

and xhform = [ `Form of (attr * string) list * xhformcont list ]

and xhalink = [ `A of (attr * string) list * xhacont list ]

and xhimg = [ `Img of (attr * string) list * xhnotag list ]

and xhheadlink = [ `Link of (attr * string) list * xhpcdata list ]

and xhscript = [ `Script of (attr * string) list * xhpcdata list ]

and xhinput = [ `Input of (attr * string) list * xhnotag list ]

and xhtextarea = [ `Textarea of (attr * string) list * xhpcdata list ]





let rec xh_search l xh_type = match l with
               (texte,xh_type2)::queue when xh_type2 = xh_type -> texte;
               | (_,_)::queue -> xh_search queue xh_type;
        | _  -> assert false;;

open Format

let xh_string = str_formatter
let taille_tab = 2

let xh_topxml = "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>"
let xh_topdoctype = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"


let rec xh_print_attrs attrs = match attrs with
  [] ->  ();
| (xh_type,texte)::queue -> pp_print_string xh_string (" "^(xh_search attrlist xh_type)^"=\""^texte^"\"");
    xh_print_attrs queue;;

let rec xh_print_pcdata texte i is_first = 
(*  pp_open_tbox xh_string ();
  if ((i > 0) || is_first) then 
    pp_force_newline xh_string ();
  if ((i > 0) || is_first) then
    pp_print_tbreak xh_string (taille_tab*i) 0; *)
  pp_print_string xh_string texte;
(*  pp_close_tbox xh_string (); *)

and xh_print_closedtag tag attrs i is_first =  pp_open_tbox xh_string ();
  if (i > 0) || is_first then 
    pp_force_newline xh_string ();
  if ((i > 0) || is_first) then
    pp_print_tbreak xh_string (taille_tab*i) 0;
  pp_print_string xh_string ("<"^tag);
  xh_print_attrs attrs;
  pp_print_string xh_string "/>";
  pp_close_tbox xh_string ();

and xh_print_inlinetag tag attrs taglist i is_first = 
(*  pp_open_tbox xh_string ();
  if (i > 0) || is_first then 
    pp_force_newline xh_string ();
  if ((i > 0) || is_first) then
    pp_print_tbreak xh_string (taille_tab*i) 0; ?????? *)
  pp_print_string xh_string ("<"^tag);
  xh_print_attrs attrs;
  pp_print_string xh_string ">";
  xh_print_taglist taglist 0 false false;
  pp_print_string xh_string ("</"^tag^">");
(*  pp_close_tbox xh_string (); *)
  
and xh_print_blocktag tag attrs taglist i = 
  if taglist = [] 
  then xh_print_closedtag tag attrs i true
  else begin
    pp_open_tbox xh_string ();
    pp_force_newline xh_string ();
    if i > 0 then
      pp_print_tbreak xh_string (taille_tab*i) 0;
    pp_print_string xh_string ("<"^tag);
    xh_print_attrs attrs;
    pp_print_string xh_string ">";
    
    xh_print_taglist_removews taglist (i+1) true;
    
    pp_force_newline xh_string ();
    if i > 0 then
      pp_print_tbreak xh_string (taille_tab*i) 0;
    pp_print_string xh_string ("</"^tag^">");
    pp_close_tbox xh_string ()
  end

and xh_print_semiblocktag tag attrs taglist i = 
  (* New line before and after but not inside, for ex for <pre> *)
  if taglist = []
  then xh_print_closedtag tag attrs i true
  else begin
    pp_open_tbox xh_string ();
    pp_force_newline xh_string ();
    if i > 0 then
      pp_print_tbreak xh_string (taille_tab*i) 0;
    pp_print_string xh_string ("<"^tag);
    xh_print_attrs attrs;
    pp_print_string xh_string ">";
    
    xh_print_taglist taglist 0 false false;

    pp_print_string xh_string ("</"^tag^">");
    pp_close_tbox xh_string ()
  end

and xh_print_taglist_removews taglist i is_first = 
  match taglist with
    (`Whitespace s)::l -> xh_print_taglist_removews l i is_first
  | l -> xh_print_taglist l i is_first true
  
and xh_print_taglist taglist i is_first removetailingws = match taglist with 
  
  (* ici on a fini la liste *)
  [] -> pp_open_tbox xh_string ();
    pp_close_tbox xh_string ();

  (* Comentaires *)
| (`Comment(texte))::queue ->
    xh_print_pcdata ("<!--"^texte^"-->") i is_first;
    xh_print_taglist queue i false removetailingws;

  (* texte version nature *)
| (`PCData(texte))::queue ->
    xh_print_pcdata texte i is_first;
    xh_print_taglist queue i false removetailingws;

  (* Balises n'ayant aucun tag a l'interieur *)
| (`Br(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "br" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Base(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "base" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Meta(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "meta" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Link(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "link" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Hr(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Hr(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Hr(xh_attrs,xh_taglist))::queue
| (`Hr(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "hr" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Param(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "param" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Img(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "img" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Area(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "area" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Input(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "input" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Col(xh_attrs,xh_taglist))::queue ->
    xh_print_closedtag "col" xh_attrs i is_first;
    xh_print_taglist queue i false removetailingws;
    
    (* Balises de presentation, type inline *)
| (`B(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "b" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Big(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "big" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`I(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "i" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Small(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "small" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Sub(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "sub" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Sup(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "sup" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Tt(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "tt" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

    (* Balises de formulaire de type block *)
| (`Whitespace _)::(`Fieldset(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Fieldset(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Fieldset(xh_attrs,xh_taglist))::queue
| (`Fieldset(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "fieldset" xh_attrs (xh_taglist : xhfieldsetcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Form(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Form(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Form(xh_attrs,xh_taglist))::queue
| (`Form(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "form" xh_attrs (xh_taglist : xhformcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

    (* Balise de formulaire de type inline*)
| (`Select(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "select" xh_attrs (xh_taglist : xhselectcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;        

| (`Button(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "button" xh_attrs (xh_taglist : xhbuttoncont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Label(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "label" xh_attrs (xh_taglist : xhlabelcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Legend(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "legend" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Optgroup(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "optgroup" xh_attrs (xh_taglist : xhoptgroupcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Option(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "option" xh_attrs (xh_taglist : xhpcdata list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Textarea(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "textarea" xh_attrs (xh_taglist : xhpcdata list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

    (* Balise de texte de type inline *)
| (`A(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "a" xh_attrs (xh_taglist : xhacont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Abbr(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "abbr" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Acronym(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "acronym" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Cite(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "cite" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Code(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "code" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Dfn(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "dfn" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Em(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "em" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Kbd(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "kbd" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Q(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "q" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Samp(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "samp" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Span(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "span" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Strong(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "strong" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
| (`Var(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "var" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

    (* Balise de division de type block *)
| (`Whitespace _)::(`Address(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Address(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Address(xh_attrs,xh_taglist))::queue
| (`Address(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "address" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Body(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Body(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Body(xh_attrs,xh_taglist))::queue
| (`Body(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "body" xh_attrs (xh_taglist : xhbodycont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Head(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Head(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Head(xh_attrs,xh_taglist))::queue
| (`Head(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "head" xh_attrs (xh_taglist : xhheadcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Blockquote(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Blockquote(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Blockquote(xh_attrs,xh_taglist))::queue
| (`Blockquote(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "blockquote" xh_attrs (xh_taglist : xhblockquotecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Div(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Div(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Div(xh_attrs,xh_taglist))::queue
| (`Div(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "div" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Html(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Html(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Html(xh_attrs,xh_taglist))::queue
| (`Html(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "html" xh_attrs (xh_taglist : xhtmlcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`H1(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`H1(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`H1(xh_attrs,xh_taglist))::queue
| (`H1(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "h1" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`H2(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`H2(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`H2(xh_attrs,xh_taglist))::queue
| (`H2(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "h2" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`H3(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`H3(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`H3(xh_attrs,xh_taglist))::queue
| (`H3(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "h3" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`H4(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`H4(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`H4(xh_attrs,xh_taglist))::queue
| (`H4(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "h4" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`H5(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`H5(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`H5(xh_attrs,xh_taglist))::queue
| (`H5(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "h5" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`H6(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`H6(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`H6(xh_attrs,xh_taglist))::queue
| (`H6(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "h6" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`P(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`P(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`P(xh_attrs,xh_taglist))::queue
| (`P(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "p" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Pre(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Pre(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Pre(xh_attrs,xh_taglist))::queue
| (`Pre(xh_attrs,xh_taglist))::queue ->
    xh_print_semiblocktag "pre" xh_attrs (xh_taglist : xhprecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

    (* Balise interactif de type block *)
| (`Map(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "map" xh_attrs (xh_taglist : xhmapcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Noscript(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "noscript" xh_attrs (xh_taglist : xhbodycont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Object(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "object" xh_attrs (xh_taglist : xhobjectcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Script(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "script" xh_attrs (xh_taglist : xhpcdata list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

    (* Balise d'entete de type inline *)
| (`Whitespace _)::(`Title(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Title(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Title(xh_attrs,xh_taglist))::queue
| (`Title(xh_attrs,xh_taglist))::queue ->
    xh_print_semiblocktag "title" xh_attrs (xh_taglist : xhpcdata list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Style(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Style(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Style(xh_attrs,xh_taglist))::queue
| (`Style(xh_attrs,xh_taglist))::queue ->
    xh_print_semiblocktag "style" xh_attrs (xh_taglist : xhpcdata list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

    (* Balise de liste de type inline *)
| (`Dt(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "dt" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

    (* Balise de liste de type block *)
| (`Dd(xh_attrs,xh_taglist))::queue ->
    
    xh_print_blocktag "dd" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Dl(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Dl(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Dl(xh_attrs,xh_taglist))::queue
| (`Dl(xh_attrs,xh_taglist))::queue -> 
    xh_print_blocktag "dl" xh_attrs (xh_taglist : xhdlcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Li(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Li(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Li(xh_attrs,xh_taglist))::queue
| (`Li(xh_attrs,xh_taglist))::queue -> 
    xh_print_blocktag "li" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Ol(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Ol(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Ol(xh_attrs,xh_taglist))::queue
| (`Ol(xh_attrs,xh_taglist))::queue -> 
    xh_print_blocktag "ol" xh_attrs (xh_taglist : xhulcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Ul(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Ul(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Ul(xh_attrs,xh_taglist))::queue
| (`Ul(xh_attrs,xh_taglist))::queue -> 
    xh_print_blocktag "ul" xh_attrs (xh_taglist : xhulcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

    (* Balises de tableau de type inline *)
| (`Caption(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "caption" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;
    
    (* Balises de tableau de type block *)
| (`Whitespace _)::(`Colgroup(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Colgroup(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Colgroup(xh_attrs,xh_taglist))::queue
| (`Colgroup(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "colgroup" xh_attrs (xh_taglist : xhcolgroupcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Table(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Table(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Table(xh_attrs,xh_taglist))::queue
| (`Table(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "table" xh_attrs (xh_taglist : xhtablecont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Tbody(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Tbody(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Tbody(xh_attrs,xh_taglist))::queue
| (`Tbody(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "tbody" xh_attrs (xh_taglist : xhtbodycont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Tfoot(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Tfoot(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Tfoot(xh_attrs,xh_taglist))::queue
| (`Tfoot(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "tfoot" xh_attrs (xh_taglist : xhtbodycont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Thead(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Thead(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Thead(xh_attrs,xh_taglist))::queue
| (`Thead(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "thead" xh_attrs (xh_taglist : xhtbodycont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Td(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Td(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Td(xh_attrs,xh_taglist))::queue
| (`Td(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "td" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;

| (`Whitespace _)::(`Th(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Th(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Th(xh_attrs,xh_taglist))::queue
| (`Th(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "th" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
| (`Whitespace _)::(`Tr(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Tr(xh_attrs,xh_taglist))::(`Whitespace _)::queue
| (`Whitespace _)::(`Tr(xh_attrs,xh_taglist))::queue
| (`Tr(xh_attrs,xh_taglist))::queue ->
    xh_print_blocktag "tr" xh_attrs (xh_taglist : xhtrcont list :> xhalltagsl) i;
    xh_print_taglist queue i false removetailingws;
    
    (* Balises divers de type inline *)
| (`Bdo(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "bdo" xh_attrs (xh_taglist : xhinlinecont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Del(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "del" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

| (`Ins(xh_attrs,xh_taglist))::queue ->
    xh_print_inlinetag "ins" xh_attrs (xh_taglist : xhdivcont list :> xhalltagsl) i is_first;
    xh_print_taglist queue i false removetailingws;

  (* espaces et retours-chariots *)
| [`Whitespace(texte)] ->
    if not removetailingws
    then pp_print_string xh_string texte

| (`Whitespace(texte))::queue ->
    pp_print_string xh_string texte;
    xh_print_taglist queue i false removetailingws;


    and xh_print (arbre : xhtml)  = 
        pp_open_tbox xh_string ();
    (* pp_print_string xh_string xh_topxml; Does not work with IE ... *)
    pp_force_newline xh_string ();
    pp_print_string xh_string xh_topdoctype;
    pp_force_newline xh_string ();
    
    xh_print_taglist ([arbre] :> xhalltagsl) 0 true false;

    pp_force_newline xh_string ();
    pp_close_tbox xh_string ();

    flush_str_formatter ()

