type descr = {
  interface_only : string list;
  interface : string list;
  internal : string list;
}

let server =
  {
    interface_only =
      [
        "eliom_content_sigs";
        "eliom_form_sigs";
        "eliom_parameter_sigs";
        "eliom_registration_sigs";
        "eliom_service_sigs";
        "eliom_shared_sigs";
      ];
    interface =
      [
        "eliom_bus";
        "eliom_client_value";
        "eliom_syntax";
        "eliom_client";
        "eliom_comet";
        "eliom_common";
        "eliom_config";
        "eliom_content";
        "eliom_extension";
        "eliom_lib";
        "eliom_mkreg";
        "eliom_notif";
        "eliom_parameter";
        "eliom_react";
        "eliom_shared";
        "eliom_cscache";
        "eliom_reference";
        "eliom_registration";
        "eliom_request_info";
        "eliom_service";
        "eliom_state";
        "eliom_tools";
        "eliom_types";
        "eliom_uri";
        "eliom_wrap";
      ];
    internal =
      [
        "eliom_comet_base";
        "eliom_common_base";
        "eliom_runtime";
        "eliom_content_";
        "eliom_content_core";
        "eliom_cookies_base";
        "eliom_error_pages";
        "eliom_form";
        "eliom_lazy";
        "eliom_lib_base";
        "eliom_parameter_base";
        "eliom_process";
        "eliom_service_base";
        "eliom_route";
        "eliom_route_base";
        "eliom_shared_content";
        "eliom_types_base";
        "eliom_client_main";
        "eliommod";
        "eliommod_cli";
        "eliommod_cookies";
        "eliommod_datasess";
        "eliommod_gc";
        "eliommod_pagegen";
        "eliommod_parameters";
        "eliommod_persess";
        "eliommod_sersess";
        "eliommod_sessadmin";
        "eliommod_sessexpl";
        "eliommod_sessiongroups";
        "eliommod_timeouts";
      ];
  }

let client =
  {
    interface_only =
      [
        "eliom_content_sigs";
        "eliom_form_sigs";
        "eliom_parameter_sigs";
        "eliom_registration_sigs";
        "eliom_service_sigs";
        "eliom_shared_sigs";
      ];
    interface =
      [
        "eliom_bus";
        "eliom_client_value";
        "eliom_client_core";
        "eliom_client";
        "eliom_comet";
        "eliom_config";
        "eliom_content";
        "eliom_content_core";
        "eliom_lazy";
        "eliom_lib";
        "eliom_parameter";
        "eliom_react";
        "eliom_shared";
        "eliom_cscache";
        "eliom_registration";
        "eliom_service";
        "eliom_tools";
        "eliom_types";
        "eliom_unwrap";
        "eliom_uri";
      ];
    internal =
      [
        "eliom_comet_base";
        "eliom_common";
        "eliom_common_base";
        "eliom_runtime";
        "eliom_content_";
        "eliom_cookies_base";
        "eliom_form";
        "eliom_lib_base";
        "eliom_parameter_base";
        "eliom_process";
        "eliom_request";
        "eliom_request_info";
        "eliom_service_base";
        "eliom_route";
        "eliom_route_base";
        "eliom_shared_content";
        "eliom_types_base";
        "eliommod_cookies";
        "eliommod_dom";
        "eliommod_parameters";
      ];
  }

let server_ext =
  {
    interface_only = [];
    interface = [ "atom_feed"; "eliom_atom"; "eliom_openid"; "eliom_s2s" ];
    internal = [];
  }

let ocamlbuild =
  { interface_only = []; interface = [ "ocamlbuild_eliom" ]; internal = [] }

let ppx =
  {
    interface_only = [];
    interface =
      [ "ppx_eliom"; "ppx_eliom_client"; "ppx_eliom_type"; "ppx_eliom_server" ];
    internal = [ "ppx_eliom_utils" ];
  }

let ( -.- ) name ext = name ^ "." ^ ext

let exts el sl =
  List.flatten (List.map (fun ext -> List.map (fun name -> name -.- ext) sl) el)

let list_to_file filename list =
  let oc = open_out filename in
  List.iter
    (fun s ->
      output_string oc s;
      output_char oc '\n')
    list;
  close_out oc

let client_mllib = client.interface @ client.internal
let client_extra = exts [ "cmi" ] (client.interface_only @ client.interface)
let client_api = client.interface_only @ client.interface
let server_mllib = server.interface @ server.internal

let server_extra =
  exts [ "cmi" ] (server.interface_only @ server.interface)
  @ exts [ "cmx" ] (server.interface @ server.internal)

let server_api = server.interface_only @ server.interface
let server_ext_mllib = server_ext.interface @ server_ext.internal

let server_ext_extra =
  exts [ "cmi" ] (server_ext.interface_only @ server_ext.interface)
  @ exts [ "cmx" ] (server_ext.interface @ server_ext.internal)

let ocamlbuild_mllib = ocamlbuild.interface @ ocamlbuild.internal

let ocamlbuild_extra =
  exts [ "cmi" ] (ocamlbuild.interface_only @ ocamlbuild.interface)
  @ exts [ "cmx" ] (ocamlbuild.interface @ ocamlbuild.internal)

let ocamlbuild_api = ocamlbuild.interface_only @ ocamlbuild.interface
let ppx_mllib = ppx.interface @ ppx.internal

let ppx_extra =
  exts [ "cmi" ] ppx.interface @ exts [ "cmx" ] (ppx.interface @ ppx.internal)

let ppx_api = ppx.interface
let templates_dir = "pkg/distillery/templates"
let templates = Array.to_list (Sys.readdir templates_dir)

let templates_files =
  List.map
    (fun name ->
      (name, Array.to_list (Sys.readdir (templates_dir ^ "/" ^ name))))
    templates
