(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 * Copyright (C) 2007 Gabriel Kerneis
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
(*
  Syntax extension for xml

*)
open Xhtmlparser;
open Camlp4.PreCast;
module Parser = Xhtmlparser.Make(Syntax)(struct value module_id = "XHTML"; 
  value module_types_id = "Xhtmltypes";
end);
module Parser5 = Xhtmlparser.Make(Syntax)(struct value module_id = "XHTML5"; 
  value module_types_id = "Xhtml5types";
end);
module ParserSVG = Xhtmlparser.Make(Syntax)(struct value module_id = "SVG"; 
  value module_types_id = "Svgtypes";
end);

do {
  Syntax.Quotation.add "xml" Syntax.Quotation.DynAst.expr_tag Parser.xml_exp ;
  Syntax.Quotation.add "xmllist" Syntax.Quotation.DynAst.expr_tag
        Parser.xml_expl;
  Syntax.Quotation.default.val := "xml";
  Syntax.Quotation.add "xhtml5" Syntax.Quotation.DynAst.expr_tag Parser5.xml_exp ;
  Syntax.Quotation.add "xhtml5list" Syntax.Quotation.DynAst.expr_tag
        Parser5.xml_expl;
  Syntax.Quotation.add "svg" Syntax.Quotation.DynAst.expr_tag ParserSVG.xml_exp ;
  Syntax.Quotation.add "svglist" Syntax.Quotation.DynAst.expr_tag
        ParserSVG.xml_expl
};

