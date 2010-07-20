(* Ocsigen
 * Copyright (C) 2008 Vincent Balat, Mauricio Fernandez
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

open XML
open Buffer

module Compact = Xhtml_mkcompact.MakeCompact (struct
  include XHTML5.M
  let default_doctype = `XHTML_05_00
  let doctype = XHTML5.M.doctype
  let emptytags = ["hr"; "br"; "img"; "meta"; "link"; "input";
                  "col"; "area"; "param"; "base"; "basefont";
                  "isindex"; "frame"]
    
 (* as per XHTML 1.0, appendix C.8; name attr deprecated in 1.1 *)
  let need_name = ["a"; "applet"; "form"; "frame"; "iframe"; "img"; "map"]
 

end)
include Compact
