(* Ocsigen
 * http://www.ocsigen.org
 * File ocsigen_loader.ml
 * Copyright (C) 2008 Stéphane Glondu
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

exception Dynlink_error of string * exn
exception Findlib_error of string * exn


(************************************************************************)
(* Loading files *)

let isloaded, addloaded =
  let set = ref Ocsigen_lib.StringSet.empty in
  ((fun s -> Ocsigen_lib.StringSet.mem s !set),
   (fun s -> set := Ocsigen_lib.StringSet.add s !set))


let loadfile pre post force file =
  try
    if force then begin
      pre ();
      Messages.debug (fun () -> "Loading "^file^" (will be reloaded every times)");
      begin try
        Dynlink.loadfile file; post ()
      with e ->
        post (); raise e
      end
    end
    else if not (isloaded file) then begin
      pre ();
      Messages.debug (fun () -> "Loading extension "^file);
      begin try
        Dynlink.loadfile file; post ()
      with e ->
        post (); raise e
      end;
      addloaded file;
    end
    else
      Messages.debug (fun () -> "Extension "^file^" already loaded")
  with
    | e -> raise (Dynlink_error (file, e))


let id () = ()


let loadfiles pre post force modules =
  let rec aux = function
    | [] -> ()
    | [m] -> loadfile pre post force m
    | m::q -> loadfile id id false m; aux q
  in aux modules


(************************************************************************)
(* Manipulating Findlib's search path *)

let () = Findlib.init ()
let ocsigen_search_path = ref []

let update_search_path () =
  match !ocsigen_search_path with
    | [] -> Findlib.init ()
    | x -> Findlib.init ~env_ocamlpath:(String.concat ":" x) ()

let get_ocamlpath = Findlib.search_path

let set_ocamlpath lp =
  ocsigen_search_path := lp;
  update_search_path ()

let add_ocamlpath p =
  ocsigen_search_path := p :: !ocsigen_search_path;
  update_search_path ()


(************************************************************************)
(* Using Findlib to locate files *)

let findfiles =
  let cmx = Netstring_pcre.regexp_case_fold "\\.cmx($| |a)" in
  fun package ->
    try
      let preds = [(if Ocsigen_config.is_native then "native" else "byte"); "plugin"; "mt"] in
      let deps = Findlib.package_deep_ancestors preds [package] in
      let deps = List.filter
        (fun a -> not (Ocsigen_lib.StringSet.mem a Ocsigen_config.builtin_packages)) deps in
      Messages.debug
        (fun () ->
           Printf.sprintf "Dependencies of %s: %s" package (String.concat ", " deps));
      let rec aux = function
        | [] -> []
        | a::q ->
            let mods =
              try
                let raw = Findlib.package_property preds a "archive" in
                (* Replacing .cmx/.cmxa by .cmxs *)
                let raw = Netstring_pcre.global_replace cmx ".cmxs " raw in
                List.filter ((<>) "") (Ocsigen_lib.split ~multisep:true ' ' raw)
              with
                | Not_found -> []
            in
            let base = Findlib.package_directory a in
            (List.map (Findlib.resolve_path ~base) mods) @ (aux q)
      in
      let res = aux deps in
      Messages.debug
        (fun () ->
           Printf.sprintf "Needed: %s" (String.concat ", " res));
      res
    with
      | e -> raise (Findlib_error (package, e))


(************************************************************************)
(* Error formatting *)

open Printf

let rec error_message = function
  | Dynlink.Error e -> Dynlink.error_message e
  | Dynlink_error (s, e) -> sprintf "Dynlink error while loading %s: %s" s (error_message e)
  | Findlib_error (s, e) -> sprintf "Findlib error while handling %s: %s" s (error_message e)
  | e -> Printexc.to_string e
