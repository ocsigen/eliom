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
(* The table of static pages for each virtual server                         *)
type assockind =
  | Dir of string * bool
  | Regexp of Netstring_pcre.regexp *
        Ocsigen_extensions.ud_string *
        bool *
        Netstring_pcre.regexp option



(*****************************************************************************)
(* Finding files *)

type res =
  | RFile of string
  | RDir of string

let code_match regexp code =
  match regexp with
  | None -> true
  | Some regexp ->
      Netstring_pcre.string_match regexp (string_of_int code) 0 <> None

let find_static_page dir err path pathstring =
  let find_file (filename, readable) =
    (* See also module Files in eliom.ml *)
    try
      Ocsigen_messages.debug (fun () -> "--Staticmod: Testing \""^filename^"\".");
      let stat = Unix.LargeFile.stat filename in
      let (filename, stat) =
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then
          (if (filename.[(String.length filename) - 1]) = '/'
          then
            let fn2 = filename^"index.html" in
            Ocsigen_messages.debug (fun () -> "--Staticmod: Testing \""^fn2^"\".");
	    try
	      (fn2, (Unix.LargeFile.stat fn2))
	    with
	    | Unix.Unix_error (Unix.ENOENT,_,_) ->
	        if readable
	        then (filename, stat)
	        else raise Failed_403
          else
            (if (path= []) || (path = [""])
            then
              let fn2 = filename^"/index.html" in
              Ocsigen_messages.debug (fun () -> "--Staticmod: Testing \""^fn2^"\".");
              try
	        (fn2, (Unix.LargeFile.stat fn2))
	      with
	      | Unix.Unix_error (Unix.ENOENT, _, _) ->
		  if readable
		  then (filename^"/", stat)
		  else raise Failed_403
            else (Ocsigen_messages.debug
                    (fun () -> "--Staticmod: "^filename^" is a directory");
                  raise Ocsigen_Is_a_directory)))
        else (filename, stat)
      in
      Ocsigen_messages.debug
        (fun () -> "--Staticmod: Looking for \""^filename^"\".");
      if (stat.Unix.LargeFile.st_kind = Unix.S_REG)
      then begin
        Unix.access filename [Unix.R_OK];
        RFile filename
      end
      else (
        if (stat.Unix.LargeFile.st_kind = Unix.S_DIR)
        then
	  RDir filename
        else raise Failed_404)
    with Unix.Unix_error (Unix.ENOENT,_,_) -> raise Failed_404
  in

  match dir with
  | Dir (d, readable) ->
      (None, find_file ((d^"/"^pathstring), readable))
  | Regexp (regexp, dest, readable, code) ->
      if code_match code err then
        (code,
         match Netstring_pcre.string_match regexp pathstring 0 with
         | None -> raise Not_concerned
         | Some _ -> (* Matching regexp found! *)
             find_file
               ((try
                 Ocsigen_extensions.replace_user_dir regexp dest pathstring
               with Not_found -> raise Failed_404),
                readable))
      else raise Not_concerned






let gen dir charset = function
  | Ocsigen_extensions.Req_found (_, r) -> Lwt.return (Ocsigen_extensions.Ext_found r)
  | Ocsigen_extensions.Req_not_found (err, ri) ->
      catch
        (* Is it a static page? *)
        (fun () ->
           if ri.ri_get_params_string = None
             (* static pages do not have parameters *)
           then begin
             Ocsigen_messages.debug2 "--Staticmod: Is it a static file?";
             match
               find_static_page
                 dir err ri.ri_sub_path ri.ri_sub_path_string
             with
               | code, RDir dirname ->
                   Ocsigen_senders.Directory_content.result_of_content
                     (dirname, ri.ri_full_path) >>= fun r ->
                   (match code with
                      | None -> return (Ext_found (fun () -> Lwt.return r))
                      | Some _ -> (* It is an error handler *)
                          return
                            (Ext_found
                               (fun () ->
                                  Lwt.return
                                    {r with
		                       Ocsigen_http_frame.res_code= err;
                                    })))
                     | code, RFile filename ->
                         Ocsigen_senders.File_content.result_of_content filename
                         >>= fun r ->
                         (match code with
                            | None ->
                                return
                                  (Ext_found
                                     (fun () ->
                                        Lwt.return
                                          {r with
		                             Ocsigen_http_frame.res_charset= Some charset;
                                          }))
                            | Some _ -> (* It is an error handler *)
                                return
                                  (Ext_found
                                     (fun () ->
                                        Lwt.return
                                          {r with
		                             Ocsigen_http_frame.res_charset= Some charset;
		                             Ocsigen_http_frame.res_code= err;
                                          })))

           end
           else return (Ext_next 400))

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


let parse_config path charset _ parse_site =
  let rec parse_attrs ((dir, regexp, readable, code, dest) as res) = function
    | [] -> res
    | ("dir", d)::l when dir = None ->
        parse_attrs
          (Some d, regexp, readable, code, dest)
          l
    | ("readable", "readable")::l when readable = None ->
        parse_attrs (dir, regexp, Some true, code, dest) l
    | ("code", c)::l when code = None ->
        (try
          parse_attrs
            (dir, regexp, readable, Some (Netstring_pcre.regexp ("^"^c^"$")), dest)
            l
        with Failure _ ->
          raise (Error_in_config_file "Bad regexp in <static code=\"...\" />"))
    | ("regexp", s)::l when regexp = None ->
        (try
          parse_attrs
            (dir, Some (Netstring_pcre.regexp ("^"^s^"$")),
             readable, code, dest)
               l
        with Failure _ ->
          raise (Error_in_config_file "Bad regexp in <static regexp=\"...\" />"))
    | ("dest", s)::l when dest = None ->
        parse_attrs
          (dir, regexp, readable, code, Some (parse_user_dir s))
          l
    | _ -> raise (Error_in_config_file "Wrong attribute for <static>")
  in
  function
    | Element ("static", atts, []) ->
        let info =
          let ((_, _, readable, _, _) as r) =
            parse_attrs (None, None, None, None, None) atts
          in
          let readable = match readable with
          | Some r -> r
          | None -> false
          in
          match r with
          | (None, None, _, None, _) ->
              raise (Error_in_config_file
                       "Missing attribute dir, regexp, or code for <static>")
          | (Some d, None, _, None, None) ->
              Dir (remove_end_slash d, readable)
          | (None, Some r, _, code, Some t) ->
              Regexp (r, t, readable, code)
          | (None, None, _, (Some _ as code), Some t) ->
              Regexp (Netstring_pcre.regexp "^.*$", t, readable, code)
          | _ -> raise (Error_in_config_file "Wrong attributes for <static>")
        in
        gen info charset
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> raise (Error_in_config_file "(staticmod extension) Bad data")


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

