type descr = {
  interface : string list;
  internal : string list;
}

let server = {
  interface = [
    "eliom_bus";
    "eliom_comet";
    "eliom_common";
    "eliom_config";
    "eliom_content";
    "eliom_cookie";
    "eliom_extension";
    "eliom_lib";
    "eliom_mkforms";
    "eliom_mkreg";
    "eliom_parameter";
    "eliom_pervasives";
    "eliom_react";
    "eliom_csreact";
    "eliom_reference";
    "eliom_registration";
    "eliom_request_info";
    "eliom_service";
    "eliom_state";
    "eliom_tools";
    "eliom_tools_common";
    "eliom_types";
    "eliom_uri";
    "eliom_wrap";
  ];
  internal = [
    "eliom_comet_base";
    "eliom_common_base";
    "eliom_content_";
    "eliom_content_core";
    "eliom_cookies_base";
    "eliom_error_pages";
    "eliom_lazy";
    "eliom_lib_base";
    "eliom_parameter_base";
    "eliom_pervasives_base";
    "eliom_process";
    "eliom_registration_base";
    "eliom_service_base";
    "eliom_types_base";
    "eliommod";
    "eliommod_cli";
    "eliommod_cookies";
    "eliommod_datasess";
    "eliommod_gc";
    "eliommod_naservices";
    "eliommod_pagegen";
    "eliommod_parameters";
    "eliommod_persess";
    "eliommod_sersess";
    "eliommod_services";
    "eliommod_sessadmin";
    "eliommod_sessexpl";
    "eliommod_sessiongroups";
    "eliommod_timeouts";
  ]
}
let client = {
  interface = [
    "eliom_bus";
    "eliom_client";
    "eliom_comet";
    "eliom_config";
    "eliom_content";
    "eliom_content_core";
    "eliom_lazy";
    "eliom_lib";
    "eliom_mkforms";
    "eliom_parameter";
    "eliom_pervasives";
    "eliom_react";
    "eliom_csreact";
    "eliom_registration";
    "eliom_service";
    "eliom_tools";
    "eliom_types";
    "eliom_unwrap";
    "eliom_uri";
  ];
  internal = [
    "eliom_comet_base";
    "eliom_common";
    "eliom_common_base";
    "eliom_content_";
    "eliom_cookies_base";
    "eliom_lib_base";
    "eliom_parameter_base";
    "eliom_pervasives_base";
    "eliom_process";
    "eliom_registration_base";
    "eliom_request";
    "eliom_request_info";
    "eliom_service_base";
    "eliom_types_base";
    "eliommod_cookies";
    "eliommod_dom";
    "eliommod_jstable";
    "eliommod_parameters";
  ];
}

let server_ext = {
  interface = [
    "atom_feed";
    "eliom_atom";
    "eliom_openid";
    "eliom_s2s"];
  internal = []
}

let ocamlbuild = {
  interface = [ "ocamlbuild_eliom" ];
  internal = []

}

let (-.-) name ext = name ^ "." ^ ext
let exts el sl =
  List.flatten (
    List.map (fun ext ->
        List.map (fun name ->
            name -.- ext) sl) el)

let list_to_file filename list =
  let oc = open_out filename in
  List.iter (fun s ->
      output_string oc s;
      output_char oc '\n';
    ) list;
  close_out oc;;

let client_mllib =
  client.interface @ client.internal

let client_extra = exts ["cmi"] client.interface

let client_api =
  let names = client.interface in
  names

let server_mllib =
  server.interface @ server.internal

let server_extra =
  exts ["cmi"] server.interface @
  exts ["cmx"] (server.interface @ server.internal)

let server_api =
  let names =
    server.interface @
    List.map (fun e -> "extensions/" ^ e) server_ext.interface
  in
  names

let server_ext_mllib = server_ext.interface @ server_ext.internal
let server_ext_extra =
  exts ["cmi"] server_ext.interface @
  exts ["cmx"] (server_ext.interface @ server_ext.internal)

let ocamlbuild_mllib = ocamlbuild.interface @ ocamlbuild.internal
let ocamlbuild_extra =
  exts ["cmi"] ocamlbuild.interface @
  exts ["cmx"] (ocamlbuild.interface @ ocamlbuild.internal)
let ocamlbuild_api = ocamlbuild.interface


let templates_dir = "pkg/distillery"
let templates = Array.to_list (Sys.readdir templates_dir)
let templates_files =
  List.map (fun name ->
    name, Array.to_list (Sys.readdir (templates_dir^"/"^name))) templates
