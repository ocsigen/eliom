(* Ocsigen
 * http://www.ocsigen.org
 * Module staticmod.ml
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
(*****************************************************************************)
(*****************************************************************************)
(* Ocsigen module to load static pages                                       *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt
open Ocsigen_lib
open Ocsigen_extensions

exception Not_concerned


(*****************************************************************************)
(* Structures describing the static pages a each virtual server *)

(* A static site is either an entire directory served unconditionnaly,
   or a more elaborate redirection based on regexpes and http error
   codes. See the documentation of staticmod for detail *)
type static_kind =
  | Dir of string (* Serves an entire directory *)
  | Regexp of regexp_site
and regexp_site = {
  source_regexp: Netstring_pcre.regexp;
  dest: Ocsigen_extensions.ud_string;
  http_status_filter: Netstring_pcre.regexp option
}



(* For both kind of static sites, we specify whether the content of
   a directory should be listed when a url is a directory, and how
   symlinks should be followed*)
type static_site = {
  static_kind: static_kind;
  options: LocalFiles.options;
}


(*****************************************************************************)
(* Finding files *)

(* Does the http status code returned for the page match the given filter ? *)
let http_status_match status_filter status =
  match status_filter with
  | None -> true
  | Some r ->
      Netstring_pcre.string_match r (string_of_int status) 0 <> None


(* Checks that the path specified in a userconf is correct.
   Currently, we check that the path does not contain ".." *)
let correct_user_local_file =
  let regexp = Netstring_pcre.regexp "(/\\.\\./)|(/\\.\\.$)" in
  fun path ->
    try ignore(Netstring_pcre.search_forward regexp path 0); false
    with Not_found -> true


(* Find the local file corresponding to [path] in the static site [dir],
   with [err] as the current http status (in case [dir] is a filter).
   Raises [Not_Concerned] if [dir] does not match, or returns
   - a boolean indicating that [dir] is an error handler
   - the local file
   If the parameter [usermode] is true, we check that the path
   is valid.
*)
let find_static_page ~usermode ~dir ~err ~pathstring =
  let status_filter, file = match dir.static_kind with
    | Dir d ->
        (false, Filename.concat d pathstring)
    | Regexp { source_regexp = source; dest = dest;
               http_status_filter = status_filter}
          when http_status_match status_filter err ->
        (status_filter <> None,
         match Netstring_pcre.string_match source pathstring 0 with
           | None -> raise Not_concerned
           | Some _ ->
               Ocsigen_extensions.replace_user_dir source dest pathstring
        )
    | _ -> raise Not_concerned
  in
  if usermode = false || correct_user_local_file file then
    (status_filter, LocalFiles.resolve file dir.options)
  else
    raise (Ocsigen_extensions.Error_in_user_config_file
             "cannot use '..' in user paths")



let gen ~usermode dir charset = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen_extensions.Ext_found r)
  | Ocsigen_extensions.Req_not_found (err, ri) ->
      catch
        (fun () ->
           Ocsigen_messages.debug2 "--Staticmod: Is it a static file?";
           let status_filter, page = find_static_page
             ~usermode ~dir ~err ~pathstring:ri.ri_sub_path_string in
           LocalFiles.content ri.ri_full_path page
           >>= fun r ->
             Lwt.return
               {r with Ocsigen_http_frame.res_charset = Some charset}
           >>= fun answer ->
           let answer' =
             if status_filter = false then
               answer
             else
               (* The page is an error handler, we propagate
                  the original error code *)
               {answer with Ocsigen_http_frame.res_code = err }
           in Lwt.return (Ext_found (fun () -> Lwt.return answer'))
        )

        (function
           | Ocsigen_Is_a_directory -> raise Ocsigen_Is_a_directory
           | LocalFiles.Failed_403 -> return (Ext_next 403)
           | LocalFiles.Failed_404 -> return (Ext_next err)
           | Not_concerned -> return (Ext_next err)
           | e -> fail e
        )


(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser

(*VVV disabled because <site> is not mandatry any more
let (default_static_dir : (string * bool) option ref) = ref None

let set_default_static_dir s p = default_static_dir := Some (s, p)

let get_default_static_dir () = !default_static_dir

let rec parse_global_config = function
  | [] -> ()
  | (Element ("static", [("dir", di)], []))::ll ->
      set_default_static_dir (remove_end_slash di) false
  | (Element ("static", [("dir", di);("readable","readable")], []))::ll ->
      set_default_static_dir (remove_end_slash di) true
  | _ -> raise (Error_in_config_file
                  ("Unexpected content inside static config"))

let _ = parse_global_config (Ocsigen_extensions.get_config ())
*)

let bad_config s = raise (Error_in_config_file s)

let rewrite_local_path userconf path =
  match userconf with
    | None -> path
    | Some { Ocsigen_extensions.localfiles_root = root } ->
        root ^ "/" ^ path

let parse_config userconf path (charset, _, _, _) _ parse_site =
  let rec parse_attrs l ((dir, regexp, readable, code, dest, follow) as res) =
    match l with
      | [] -> res

      | ("dir", d)::l when dir = None ->
          parse_attrs l (Some (rewrite_local_path userconf d), regexp, readable, code, dest, follow)

      | (("readable", "readable") | ("listdirectorycontent","1"))::l when readable = None ->
          parse_attrs l (dir, regexp, Some true, code, dest, follow)

      | ("code", c)::l when code = None ->
          let c = try Netstring_pcre.regexp ("^"^c^"$")
           with Pcre.BadPattern _ ->
             bad_config ("Bad regexp \""^c^"\" in <static code=\"...\" />")
          in
          parse_attrs l (dir, regexp, readable, Some c, dest, follow)

      | ("regexp", s)::l when regexp = None ->
          let s = try Netstring_pcre.regexp ("^"^s^"$")
           with Pcre.BadPattern _ ->
             bad_config ("Bad regexp \""^s^"\" in <static regexp=\"...\" />")
          in
          parse_attrs l (dir, Some s, readable, code, dest, follow)

      | ("dest", s)::l when dest = None ->
          parse_attrs l
            (dir, regexp, readable, code,
             Some (parse_user_dir (rewrite_local_path userconf s)), follow)

      | ("followsymlinks", s)::l when follow = None ->
          let v = match s with
            | "never" -> LocalFiles.DoNotFollow
            | "always" ->
                if userconf = None (* Not in an userconf file *) then
                  LocalFiles.AlwaysFollow
                else
                  raise
                    (Ocsigen_extensions.Error_in_user_config_file
                       "Cannot specify value 'always' for attribute 'followsymlinks' in userconf files")
            | "ownermatch" -> LocalFiles.FollowIfOwnerMatch
            | _ ->
                bad_config ("Wrong value \""^s^"\" for tag \"followsymlink\"")
          in
          parse_attrs l
            (dir, regexp, readable, code, dest, Some v)

    | _ -> bad_config "Wrong attribute for <static>"
  in
  function
    | Element ("static", atts, []) ->
        let dir, regexp, readable, code, dest, follow =
          parse_attrs atts (None, None, None, None, None, None)
        in
        let readable = (readable = Some true) (* default value is false *)
        and follow_symlinks = match follow with
          | Some v -> v
          | None -> LocalFiles.DoNotFollow (* default value *)
        in
        let kind =  match dir, regexp, code, dest with
          | (None, None, None, _) ->
              raise (Error_in_config_file
                       "Missing attribute dir, regexp, or code for <static>")
          | (Some d, None, None, None) ->
              Dir (remove_end_slash d)
          | (None, Some r, code, Some t) ->
              Regexp { source_regexp = r; dest = t; http_status_filter = code }
          | (None, None, (Some _ as code), Some t) ->
              Regexp { dest = t; http_status_filter = code;
                       source_regexp = Netstring_pcre.regexp "^.*$" }
          | _ -> raise (Error_in_config_file "Wrong attributes for <static>")
          in
          gen ~usermode:(userconf <> None)
            { static_kind = kind;
              options = {
                LocalFiles.list_directory_content = readable;
                follow_symlinks = follow_symlinks } }
            charset
    | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
    | _ -> bad_config "(staticmod extension) Bad data"



(*****************************************************************************)
(** extension registration *)
let _ = register_extension
  ~fun_site:(fun _ -> parse_config None)
  ~user_fun_site:(fun path _ -> parse_config (Some path))
  ()
