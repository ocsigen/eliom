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
module Parser = Xhtmlparser.Make(Syntax);
do {
  Syntax.Quotation.add "xml" Syntax.Quotation.DynAst.expr_tag Parser.xml_exp ;
  Syntax.Quotation.add "xmllist" Syntax.Quotation.DynAst.expr_tag
        Parser.xml_expl;
  Syntax.Quotation.default.val := "xml"
};

