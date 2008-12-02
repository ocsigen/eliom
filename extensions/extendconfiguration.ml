open Lwt
open Ocsigen_extensions
open Simplexmlparser


let bad_config s = raise (Error_in_config_file s)

let gen ~usermode configfun = function
  | Ocsigen_extensions.Req_found _ ->
      Lwt.return Ocsigen_extensions.Ext_do_nothing

  | Ocsigen_extensions.Req_not_found (err, request) ->
      Ocsigen_messages.debug2 "--Updating configuration";
      let updated_request = { request with request_config =
          configfun request.request_config }
      in
      Lwt.return
        (Ocsigen_extensions.Ext_continue_with
           (updated_request,
            Ocsigen_http_frame.Cookies.empty,
            err
           ))


let update_config usermode = function
  | Element ("listdirs", ["value", "true"], []) ->
      gen ~usermode 
        (fun config -> { config with list_directory_content = true })
  | Element ("listdirs", ["value", "false"], []) ->
      gen ~usermode 
        (fun config -> { config with list_directory_content = false })


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
      gen ~usermode 
        (fun config -> { config with follow_symlinks = v })


  | Element ("charset", attrs, exts) ->
      let rec aux charset_assoc = function
        | [] -> charset_assoc
        | Element ("extension", ["ext", extension; "value", charset], []) :: q->
            aux (update_charset ~charset_assoc ~extension ~charset) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <extension ext=\"...\" value=\"...\" /> \
                      in option charset"
      in 
      gen ~usermode 
        (fun config -> 
           let config = match attrs with
             | ["default", s] ->
                 { config with default_charset = s }
             | [] -> config
             | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"charset\""
           in
           { config with charset_assoc = aux config.charset_assoc exts })


  | Element ("contenttype", attrs, exts) ->
      let rec aux mime_assoc = function
        | [] -> mime_assoc
        | Element ("extension", ["ext", extension; "value", mime], []) :: q->
            aux (Mime.update_mime ~mime_assoc ~extension ~mime) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <extension ext=\"...\" value=\"...\" /> \
                      in option mime"
      in 
      gen ~usermode 
        (fun config -> 
           let config = match attrs with
             | ["default", s] ->
                 { config with default_mime_type = s }
             | [] -> config
             | _ -> bad_config "Only attribute \"default\" is permitted \
                           for option \"contenttype\""
           in
           { config with mime_assoc = aux config.mime_assoc exts })


  | Element ("defaultindex", [], l) ->
      let rec aux indexes = function
        | [] -> List.rev indexes
        | Element ("index", [], [PCData f]) :: q ->
            aux (f :: indexes) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <index>...</index> \
                      in option defaultindex"
      in 
      gen ~usermode 
        (fun config -> 
           { config with default_directory_index = aux [] l })

  | Element ("hidefile", [], l) ->
      let rec aux regexps = function
        | [] -> regexps
        | Element ("regexp", ["value", f], []) :: q ->
            aux (f :: regexps) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <regexp>...</regexp> \
                      in option hidefile"
      in 
      gen ~usermode 
        (fun config -> 
           { config with 
               do_not_serve_404 = aux [] l @ config.do_not_serve_404 })
           
  | Element ("forbidfile", [], l) ->
      let rec aux regexps = function
        | [] -> regexps
        | Element ("regexp", ["value", f], []) :: q ->
            aux (f :: regexps) q
        | _ :: q -> bad_config "subtags must be of the form \
                      <regexp>...</regexp> \
                      in option hidefile"
            
      in 
      gen ~usermode 
        (fun config -> 
           { config with 
               do_not_serve_403 = aux [] l @ config.do_not_serve_403 })

  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ ->
      raise (Error_in_config_file "Unexpected data in config file")


let parse_config usermode : parse_config_aux = fun _ _ _ xml ->
  update_config usermode xml


let _ = register_extension
  ~fun_site:(fun _ -> parse_config false)
  ~user_fun_site:(fun path _ -> parse_config true)
  ()
