(* Ocsigen
 * http://www.ocsigen.org
 * Module extensiontemplate.ml
 * Copyright (C) 2007 Vincent Balat
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
(*****************************************************************************)
(*****************************************************************************)
(* This is an example of extension for Ocsigen                               *)
(* Take this as a template for writing your own Eliom based
   extensions to the Web server *)
(*****************************************************************************)
(*****************************************************************************)

let _ =
  Eliom_extension.register_eliom_extension (fun sp ->
      Lwt.return
        (Ocsigen_extensions.Ext_found
           (fun () ->
             let content = "Eliom Extension template page" in
             Ocsigen_senders.Text_content.result_of_content
               (content, "text/plain"))))
