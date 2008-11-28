open Lwt
open Ocsigen_extensions
open Simplexmlparser


let bad_config s = raise (Error_in_config_file s)


let update_config usermode config = function
  | Element ("listdirs", ["value", "true"], []) ->
      { config with list_directory_content = true }
  | Element ("listdirs", ["value", "false"], []) ->
      { config with list_directory_content = false }


  | Element ("followsymlinks", ["value", s], []) ->
      let v = match s with
        | "never" -> DoNotFollowSymlinks
        | "always" ->
            if usermode = false then
              AlwaysFollowSymlinks
            else
              raise (Error_in_user_config_file
                       "Cannot specify value 'always' for option \
                        'followsymlinks' in userconf files")
        | "ownermatch" -> FollowSymlinksIfOwnerMatch
        | _ ->
            bad_config ("Wrong value \""^s^"\" for option \"followsymlinks\"")
      in
      { config with follow_symlinks = v }


  | Element ("charset", attrs, exts) ->
      let config = match attrs with
        | ["default", s] ->
            { config with default_charset = s }
        | [] -> config
        | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"charset\""
      in
      let rec aux charset_assoc = function
        | [] -> charset_assoc
        | Element ("extension", ["ext", extension; "value", charset], []) :: q->
            aux (update_charset ~charset_assoc ~extension ~charset) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <extension ext=\"...\" value=\"...\" /> \
                      in option charset"
      in { config with charset_assoc = aux config.charset_assoc exts }


  | Element ("contenttype", attrs, exts) ->
      let config = match attrs with
        | ["default", s] ->
            { config with default_mime_type = s }
        | [] -> config
        | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"contenttype\""
      in
      let rec aux mime_assoc = function
        | [] -> mime_assoc
        | Element ("extension", ["ext", extension; "value", mime], []) :: q ->
            aux (Mime.update_mime ~mime_assoc ~extension ~mime) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <extension ext=\"...\" value=\"...\" /> \
                      in option contenttype"
      in { config with mime_assoc = aux config.mime_assoc exts }


  | Element ("defaultindex", [], l) ->
      let rec aux indexes = function
        | [] -> List.rev indexes
        | Element ("index", [], [PCData f]) :: q ->
            aux (f :: indexes) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <index>...</index> \
                      in option defaultindex"
      in { config with default_directory_index = aux [] l }

  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
      raise (Error_in_config_file "Unexpected data in config file")

let gen ~usermode ~xml = function
  | Ocsigen_extensions.Req_found (_, r) ->
      Lwt.return (Ocsigen_extensions.Ext_found r)

  | Ocsigen_extensions.Req_not_found (err, request) ->
      Ocsigen_messages.debug2 "--Updating configuration";
      let updated_request = { request with request_config =
          update_config usermode request.request_config xml }
      in
      Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           (updated_request,
            Ocsigen_http_frame.Cookies.empty,
            err
           ))


let parse_config usermode : parse_site_aux = fun _ _ _ xml ->
  gen ~usermode ~xml


let _ = register_extension
  ~fun_site:(fun _ -> parse_config false)
  ~user_fun_site:(fun path _ -> parse_config true)
  ()
