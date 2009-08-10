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

let bad_config s = raise (Error_in_config_file s)


(*****************************************************************************)
(* Structures describing the static pages a each virtual server *)

(* A static site is either an entire directory served unconditionnaly,
   or a more elaborate redirection based on regexpes and http error
   codes. See the web documentation of staticmod for detail *)
type static_site_kind =
  | Dir of string (* Serves an entire directory *)
  | Regexp of regexp_site
and regexp_site = {
  source_regexp: Netstring_pcre.regexp;
  dest: Ocsigen_extensions.ud_string;
  http_status_filter: Netstring_pcre.regexp option;
  root_checks: Ocsigen_extensions.ud_string option;
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
let find_static_page ~request ~usermode ~dir ~err ~pathstring =
  let status_filter, filename, root = match dir with
    | Dir d ->
        (false,
         Filename.concat d pathstring,
         (match usermode with
            | None -> Some d
            | Some { localfiles_root = r } -> Some r
         ))
    | Regexp { source_regexp = source; dest = dest;
               http_status_filter = status_filter;
               root_checks = rc }
          when http_status_match status_filter err ->
        let status_filter = status_filter <> None
        and file =
          match Netstring_pcre.string_match source pathstring 0 with
            | None -> raise Not_concerned
            | Some _ ->
                Ocsigen_extensions.replace_user_dir source dest pathstring
        and root_checks =
         (match rc, usermode with
            | None, Some { localfiles_root = r } ->
                Some r
            | Some _, Some _ ->
                raise (Ocsigen_extensions.Error_in_user_config_file
                         "Staticmod: cannot specify option 'root' in \
                           user configuration files")
            | None, None -> None
            | Some rc, None ->
                Some (Ocsigen_extensions.replace_user_dir source rc pathstring)
         )
        in
        status_filter, file, root_checks
    | _ -> raise Not_concerned
  in
  if usermode = None || correct_user_local_file filename then
    (status_filter,
     Ocsigen_LocalFiles.resolve ?no_check_for:root ~request ~filename)
  else
    raise (Ocsigen_extensions.Error_in_user_config_file
             "Staticmod: cannot use '..' in user paths")



let gen ~usermode dir = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen_extensions.Ext_do_nothing)
  | Ocsigen_extensions.Req_not_found (err, ri) ->
      catch
        (fun () ->
           Ocsigen_messages.debug2 "--Staticmod: Is it a static file?";
           let status_filter, page =
             find_static_page ~request:ri ~usermode ~dir ~err
             ~pathstring:(Ocsigen_lib.string_of_url_path ~encode:false
                            ri.request_info.ri_sub_path) in
           Ocsigen_LocalFiles.content ri page
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
           | Ocsigen_LocalFiles.Failed_403 -> return (Ext_next 403)
               (* XXX We should try to leave an information about this
                  error for later *)
           | Ocsigen_LocalFiles.NotReadableDirectory ->
               return (Ext_next err)
           | NoSuchUser | Not_concerned
           | Ocsigen_LocalFiles.Failed_404 -> return (Ext_next err)
           | e -> fail e
        )


(*****************************************************************************)
(** Parsing of config file *)
open Simplexmlparser

(* In userconf modes, paths must be relative to the root of the userconf config *)
let rewrite_local_path userconf path =
  match userconf with
    | None -> path
    | Some { Ocsigen_extensions.localfiles_root = root } ->
        root ^ "/" ^ path

type options = {
  opt_dir: string option;
  opt_regexp: Netstring_pcre.regexp option;
  opt_code: Netstring_pcre.regexp option;
  opt_dest: Ocsigen_extensions.ud_string option;
  opt_root_checks: Ocsigen_extensions.ud_string option;
}

let parse_config userconf : parse_config_aux = fun _ _ _ ->
  let rec parse_attrs l opt =
    match l with
      | [] -> opt

      | ("dir", d)::l when opt.opt_dir = None ->
          parse_attrs l
            { opt with opt_dir = Some (rewrite_local_path userconf d)}

      | ("regexp", s)::l when opt.opt_regexp = None ->
          let s = try Netstring_pcre.regexp ("^"^s^"$")
           with Pcre.Error (Pcre.BadPattern _) ->
             bad_config ("Bad regexp \""^s^"\" in <static regexp=\"...\" />")
          in
          parse_attrs l { opt with opt_regexp = Some s }

      | ("code", c)::l when opt.opt_code = None ->
          let c = try Netstring_pcre.regexp ("^"^c^"$")
           with Pcre.Error (Pcre.BadPattern _) ->
             bad_config ("Bad regexp \""^c^"\" in <static code=\"...\" />")
          in
          parse_attrs l { opt with opt_code = Some c }

      | ("dest", s)::l when opt.opt_dest = None ->
          parse_attrs l
            { opt with opt_dest =
                Some (parse_user_dir (rewrite_local_path userconf s)) }

      | ("root", s) :: l when opt.opt_root_checks = None ->
          parse_attrs l
            { opt with opt_root_checks = Some (parse_user_dir s) }

      | _ -> bad_config "Wrong attribute for <static>"
  in
  function
    | Element ("static", atts, []) ->
        let opt =
          parse_attrs atts {
            opt_dir = None;
            opt_regexp = None;
            opt_code = None;
            opt_dest = None;
            opt_root_checks = None;
          }
        in
        let kind =
          match opt.opt_dir, opt.opt_regexp, opt.opt_code, opt.opt_dest, opt.opt_root_checks with
          | (None, None, None, _, _) ->
              raise (Error_in_config_file
                       "Missing attribute dir, regexp, or code for <static>")

          | (Some d, None, None, None, None) ->
              Dir (remove_end_slash d)

          | (None, Some r, code, Some t, rc) ->
              Regexp { source_regexp = r;
                       dest = t;
                       http_status_filter = code;
                       root_checks = rc;
                     }

          | (None, None, (Some _ as code), Some t, None) ->
              Regexp { dest = t; http_status_filter = code; root_checks = None;
                       source_regexp = Netstring_pcre.regexp "^.*$" }

          | _ -> raise (Error_in_config_file "Wrong attributes for <static>")
        in
        gen ~usermode:userconf kind
    | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
    | _ -> bad_config "(staticmod extension) Bad data"



(*****************************************************************************)
(** extension registration *)
let () = register_extension
  ~name:"staticmod"
  ~fun_site:(fun _ -> parse_config None)
  ~user_fun_site:(fun path _ -> parse_config (Some path))
  ()
