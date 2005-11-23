(* Ocsigen
 * Copyright (C) 2005 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* For the while, no config file. We use this file instead.
   Later, all these informations we be parsed from a file.
 *)

exception Config_file_error of string

let get_var = function
    "logfile" -> "../ocsigen.log"
  | "static_pages_directory" -> "../pages"
  | s -> raise (Config_file_error s)

let cmo_list =
  [
   
   (* Ocsimore : *)
   "../lib/ocsimore.cma";
   
   (* Ocsimore examples : *)
   "../lib/ocsexample_util.cmo"
     
     (*;
	(* For Ocsespace (not part of the distrib for the while) *)
	"../../ocsespace/lib/ocsespace.cmo";
      *)
     
 ]

let module_list =
  [

   (* Tutorial *)
   ([""],"../lib/moduleexample.cmo");

   (* Ocsimore examples : *)
   (["ocsimore1"],"../lib/ocsexample1.cmo");
   (["ocsimore"],"../lib/ocsexample2.cmo");
   (["ocsisav"],"../lib/ocsexample3.cmo");

   (* Profiling : *)
   (["prof"],"../lib/profiling.cmo")

   (* Ocsigen website (not part of the distrib) *)
   ;(["site"],"../../site/site_ocsigen.cmo");
   (* *)

   (*;
   (* Ocsespace (not part of the distrib for the while) *)
   (["camlcom"],"../../ocsespace/lib/camlcom.cmo")
    *)
 ]
