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


exception Failed_403
exception Failed_404
exception Not_concerned


(*****************************************************************************)
(* Structurs describing the static pages a each virtual server *)

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

type follow_symlink =
  | DoNotFollow (* Never follow a symlink *)
  | FollowIfOwnerMatch (* Follow a symlink if the symlink and its
                          target have the same owner *)
  | AlwaysFollow (* Always follow symlinks *)


(* For both kind of static sites, we specify whether the content of
   a directory should be listed when a url is a directory, and how
   symlinks should be followed*)
type static_site = {
  static_kind: static_kind;
  list_directory_content : bool;
  follow_symlinks: follow_symlink;
}


(*****************************************************************************)
(* Finding files *)

(* Return type of a request. The string argument represents the real
   file/directory to serve, eg. foo/index.html instead of foo *)
type res =
  | RFile of string
  | RDir of string

(* Does the http status code returned for the page match the given filter ? *)
let http_status_match status_filter status =
  match status_filter with
  | None -> true
  | Some r ->
      Netstring_pcre.string_match r (string_of_int status) 0 <> None


(* checks that [filename] can be followed depending on its third argument.
   [stat] must be the result of calling [Unix.stat] on filename *)
let should_follow filename stat = function
  | AlwaysFollow -> true
  | f ->
      let lstat = Unix.LargeFile.lstat filename in
      if lstat.Unix.LargeFile.st_kind = Unix.S_LNK then
        if f = DoNotFollow then
          false
        else
          stat.Unix.LargeFile.st_uid = lstat.Unix.LargeFile.st_uid
      else
        true


(* given [filename], we search for it in the local filesystem and
   - we return ["filename/index.html"] if [filename] corresponds to
     a directory, and ["filename/index.html"] is valid
   - we raise [Failed_403] if [filename] corresponds to a directory,
     ["filename/index.html"] does not exists and [list_dir_content] is false
   - we raise [Failed_403] if [filename] is a symlink that must
     not be followed
   - raises [Failed_404] if [filename] does not exist, or is a special file
   - otherwise returns [filename]
*)
(* See also module Files in eliom.ml *)
let find_file filename list_dir_content follow_symlinks =
  try
    Ocsigen_messages.debug (fun () -> "--Staticmod: Testing \""^filename^"\".");
    let stat = Unix.LargeFile.stat filename in
    let (filename, stat) =
      if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
        if filename.[String.length filename - 1] <> '/' then begin
          (* In this case, [filename] is a directory but this is not visible in
             its name as there is no final slash. We signal this fact to
             Ocsigen, which will then issue a 301 redirection to "filename/" *)
          Ocsigen_messages.debug
            (fun () -> "--Staticmod: "^filename^" is a directory");
          raise Ocsigen_Is_a_directory

        end else begin
          let fn2 = filename ^ "index.html" in
          Ocsigen_messages.debug
            (fun () -> "--Staticmod: Testing \""^fn2^"\".");
          try
            (fn2, Unix.LargeFile.stat fn2)
          with
            | Unix.Unix_error (Unix.ENOENT, _, _) ->
                if list_dir_content then (filename, stat) else raise Failed_403
        end
      else (filename, stat)
    in
    if should_follow filename stat follow_symlinks then
      begin
        Ocsigen_messages.debug
          (fun () -> "--Staticmod: Looking for \""^filename^"\".");
        if stat.Unix.LargeFile.st_kind = Unix.S_REG then begin
          Unix.access filename [Unix.R_OK];
          RFile filename
        end else if stat.Unix.LargeFile.st_kind = Unix.S_DIR then
          RDir filename
        else raise Failed_404
      end
    else
      (* [filename] is a symlink which we should not follow according
         to the current policy *)
      raise Failed_403
  with
    | Unix.Unix_error (Unix.ENOENT,_,_) -> raise Failed_404


(* Find the local file corresponding to [path] in the static site [dir],
   with [err] as the current http status (in case [dir] is a filter).
   Raises [Not_Concerned] if [dir] does not match, or returns
   - a boolean indicating that [dir] is an error handler
   - the local file
*)
let find_static_page dir err path =
  let pathstring = String.concat "/" path in
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
               (try
                  Ocsigen_extensions.replace_user_dir source dest pathstring
                with Not_found -> raise Failed_404))
    | _ -> raise Not_concerned
  in
  (status_filter, find_file file dir.list_directory_content dir.follow_symlinks)




let gen dir charset = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen_extensions.Ext_found r)
  | Ocsigen_extensions.Req_not_found (err, ri) ->
      catch
        (fun () ->
           Ocsigen_messages.debug2 "--Staticmod: Is it a static file?";
           let status_filter, page = find_static_page dir err ri.ri_sub_path in
           (match page with
              | RDir dirname ->
                  Ocsigen_senders.Directory_content.result_of_content
                    (dirname, ri.ri_full_path)
              | RFile filename ->
                  Ocsigen_senders.File_content.result_of_content filename
           )
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
           | Unix.Unix_error (Unix.EACCES,_,_)
           | Ocsigen_Is_a_directory
           | Ocsigen_malformed_url as e -> fail e
           | Failed_403 -> return (Ext_next 403)
           | Failed_404 -> return (Ext_next err)
               (*VVV I send err, not 404 ... (?) *)
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

let parse_config path (charset, _, _, _) _ parse_site =
  let rec parse_attrs l ((dir, regexp, readable, code, dest, follow) as res) =
    match l with
      | [] -> res

      | ("dir", d)::l when dir = None ->
          parse_attrs l (Some d, regexp, readable, code, dest, follow)

      | ("readable", "readable")::l when readable = None ->
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
            (dir, regexp, readable, code, Some (parse_user_dir s), follow)

      | ("followsymlinks", s)::l when follow = None ->
          let v = match s with
            | "never" -> DoNotFollow
            | "always" -> AlwaysFollow
            | "ownermatch" -> FollowIfOwnerMatch
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
          | None -> DoNotFollow (* default value *)
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
          gen { static_kind = kind; list_directory_content = readable;
                follow_symlinks = follow_symlinks } charset
    | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
    | _ -> bad_config "(staticmod extension) Bad data"


(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  ()


(*****************************************************************************)
(** extension registration *)
let _ = register_extension
  (fun hostpattern -> parse_config)
  (fun hostpattern -> parse_config)
  start_init
  end_init
  raise

