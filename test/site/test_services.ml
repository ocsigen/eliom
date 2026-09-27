module Service = Eliom.Service
module Parameter = Eliom.Parameter
module Uri = Eliom.Eliom_uri

(* Services are registered, since a site checks that all its services are. *)
let get ?https path params =
  let service =
    Service.create ?https ~path:(Service.Path path) ~meth:(Service.Get params)
      ()
  in
  Site.register service; service

let test_outside_a_site () =
  (* A program starts in the initialisation phase of Ocsigen Server, where
     services go to the default site: this is the case of services created
     outside a site, at runtime. *)
  Ocsigen.Extensions.end_initialisation ();
  match get ["a"] Parameter.unit with
  | _ -> Alcotest.fail "service created"
  | exception Eliom.Common.Site_information_not_available _ -> ()

let test_paths () =
  let path s = Uri.make_string_uri ~absolute_path:true ~service:s () in
  let paths =
    Site.init ~site_dir:["blog"] ~app:"paths" (fun () ->
      [ path (get ["a"; "b"] Parameter.unit)
      ; path (get ["dir"; ""] Parameter.unit)
      ; path (get [] Parameter.unit) ])
  in
  Alcotest.(check (list string))
    "paths"
    ["/blog/a/b"; "/blog/dir/"; "/blog/"]
    paths

let test_path_escaping () =
  (* Each segment of the path of a service is escaped. *)
  let path =
    Site.init ~site_dir:["s"] ~app:"escaping" (fun () ->
      Uri.make_string_uri ~absolute_path:true
        ~service:
          (get
             ["a b"; "c%d"; "e?f"; "g#h"; "\xc3\xa9"; "i/j"; "k&l"; "m+n"]
             Parameter.unit)
        ())
  in
  Alcotest.(check string)
    "path" "/s/a%20b/c%25d/e%3Ff/g%23h/%C3%A9/i%2Fj/k%26l/m%2Bn" path

let test_site_levels () =
  let relative, absolute =
    Site.init ~site_dir:["a"; "b"] ~app:"levels" (fun () ->
      let s = get ["c"] Parameter.unit in
      ( Uri.make_string_uri ~absolute_path:true ~service:s ()
      , Uri.make_string_uri ~absolute:true ~service:s () ))
  in
  Alcotest.(check string) "absolute path" "/a/b/c" relative;
  Alcotest.(check string)
    "absolute URL"
    (Printf.sprintf "http://%s:%d/a/b/c" Site.hostname Site.http_port)
    absolute

let test_default_protocol () =
  let config_info =
    {Site.config_info with Ocsigen.Extensions.default_protocol_is_https = true}
  in
  let https, default, http =
    Site.init ~config_info ~app:"default protocol" (fun () ->
      let s = get ["a"] Parameter.unit in
      ( Eliom.Config.default_protocol_is_https ()
      , Uri.make_string_uri ~absolute:true ~service:s ()
      , Uri.make_string_uri ~absolute:true ~https:false ~service:s () ))
  in
  Alcotest.(check bool) "protocol" true https;
  Alcotest.(check string)
    "default"
    (Printf.sprintf "https://%s:%d/a" Site.hostname Site.https_port)
    default;
  Alcotest.(check string)
    "HTTP"
    (Printf.sprintf "http://%s:%d/a" Site.hostname Site.http_port)
    http

let test_standard_ports () =
  (* The ports 80 and 443 are not written in URLs. *)
  let config_info =
    { Site.config_info with
      Ocsigen.Extensions.default_httpport = 80
    ; default_httpsport = 443 }
  in
  let http, https =
    Site.init ~config_info ~app:"standard ports" (fun () ->
      let s = get ["a"] Parameter.unit in
      ( Uri.make_string_uri ~absolute:true ~service:s ()
      , Uri.make_string_uri ~absolute:true ~https:true ~service:s () ))
  in
  Alcotest.(check string) "HTTP" ("http://" ^ Site.hostname ^ "/a") http;
  Alcotest.(check string) "HTTPS" ("https://" ^ Site.hostname ^ "/a") https

let prefix = Printf.sprintf "http://%s:%d" Site.hostname Site.http_port
let https_prefix = Printf.sprintf "https://%s:%d" Site.hostname Site.https_port

let test_absolute_urls () =
  let urls =
    Site.init ~site_dir:["blog"] ~app:"absolute" (fun () ->
      let s = get ["a"] Parameter.(int "i" ** string "s") in
      let suffix = get ["b"] Parameter.(suffix (int "i" ** string "s")) in
      let secure = get ~https:true ["c"] Parameter.unit in
      let nl =
        Parameter.make_non_localized_parameters ~prefix:"app" ~name:"n"
          (Parameter.int "x")
      in
      let uri ?absolute ?absolute_path ?https ?fragment ?nl_params service v =
        Uri.make_string_uri ?absolute ?absolute_path ?https ?fragment ?nl_params
          ~service v
      in
      [ uri ~absolute:true s (1, "x y")
      ; uri ~absolute_path:true s (1, "x")
      ; uri ~absolute:true ~https:true s (1, "x")
      ; uri ~absolute:true suffix (3, "x/y")
      ; uri ~absolute:true secure ()
      ; uri ~absolute_path:true ~fragment:"top" s (1, "x")
      ; uri ~absolute_path:true
          ~nl_params:
            (Parameter.add_nl_parameter Parameter.empty_nl_params_set nl 7)
          s (1, "x")
      ; uri ~absolute_path:true (Service.preapply ~service:s (2, "p")) () ])
  in
  Alcotest.(check (list string))
    "URLs"
    [ prefix ^ "/blog/a?s=x+y&i=1"
    ; "/blog/a?s=x&i=1"
    ; https_prefix ^ "/blog/a?s=x&i=1"
    ; prefix ^ "/blog/b/3/x%2Fy"
    ; https_prefix ^ "/blog/c"
    ; "/blog/a?s=x&i=1#top"
    ; "/blog/a?s=x&i=1&__nl_n_app-n.x=7"
    ; "/blog/a?s=p&i=2" ]
    urls

let test_coservice_urls () =
  let urls =
    Site.init ~app:"coservices" (fun () ->
      let fallback = get ["a"] Parameter.unit in
      let anonymous =
        Service.create_attached_get ~fallback ~get_params:(Parameter.int "i") ()
      in
      let named =
        Service.create_attached_get ~name:"named" ~fallback
          ~get_params:Parameter.unit ()
      in
      Site.register anonymous;
      Site.register named;
      ( Uri.make_string_uri ~absolute_path:true ~service:anonymous 1
      , Uri.make_string_uri ~absolute_path:true ~service:named () ))
  in
  let anonymous, named = urls in
  let path, params =
    match String.split_on_char '?' anonymous with
    | [path; params] -> path, String.split_on_char '&' params
    | _ -> Alcotest.failf "no parameters in %S" anonymous
  in
  Alcotest.(check string) "anonymous path" "/a" path;
  let state, others =
    List.partition
      (String.starts_with ~prefix:(Eliom.Common.get_numstate_param_name ^ "="))
      params
  in
  Alcotest.(check int) "state parameter" 1 (List.length state);
  (* Parameters of attached coservices are prefixed, not to be mixed with
     those of the fallback. *)
  Alcotest.(check (list string))
    "parameters"
    [Eliom.Common.co_param_prefix ^ "i=1"]
    others;
  Alcotest.(check string)
    "named"
    ("/a?" ^ Eliom.Common.get_state_param_name ^ "=named")
    named

(* [state url] is the value of the state parameter of an anonymous attached
   coservice in [url]. *)
let state url =
  let prefix = Eliom.Common.get_numstate_param_name ^ "=" in
  match
    List.find_opt
      (String.starts_with ~prefix)
      (String.split_on_char '&'
         (List.nth (String.split_on_char '?' url @ [""]) 1))
  with
  | Some p ->
      String.sub p (String.length prefix)
        (String.length p - String.length prefix)
  | None -> Alcotest.failf "no state in %S" url

let test_anonymous_coservices () =
  let first, again, second =
    Site.init ~app:"anonymous coservices" (fun () ->
      let fallback = get ["a"] Parameter.unit in
      let anonymous () =
        let s =
          Service.create_attached_get ~fallback ~get_params:Parameter.unit ()
        in
        Site.register s; s
      in
      let url s = Uri.make_string_uri ~absolute_path:true ~service:s () in
      let first = anonymous () in
      url first, url first, url (anonymous ()))
  in
  Alcotest.(check string) "same coservice" (state first) (state again);
  Alcotest.(check bool) "other coservice" false (state first = state second)

let test_post_urls () =
  (* The URL of a POST service and its POST parameters *)
  let show (path, get_params, fragment, post_params) =
    let names = List.map fst in
    path, get_params, fragment, names post_params, post_params
  in
  let attached, with_get =
    Site.init ~app:"POST" (fun () ->
      let fallback = get ["a"] Parameter.unit in
      let attached =
        Service.create_attached_post ~fallback
          ~post_params:(Parameter.string "v") ()
      in
      let with_get =
        Service.create ~path:(Service.Path ["p"])
          ~meth:(Service.Post (Parameter.int "g", Parameter.string "x"))
          ()
      in
      Site.register attached;
      Site.register with_get;
      ( show
          (Uri.make_post_uri_components ~absolute_path:true ~service:attached ()
             "x")
      , show
          (Uri.make_post_uri_components ~absolute_path:true ~service:with_get 3
             "z") ))
  in
  let path, get_params, fragment, names, post_params = attached in
  Alcotest.(check string) "attached, path" "/a" path;
  Alcotest.(check (list (pair string string)))
    "attached, GET parameters" [] get_params;
  Alcotest.(check (option string)) "attached, fragment" None fragment;
  (* The parameters of attached coservices are prefixed, and the state of an
     anonymous POST coservice is a POST parameter. *)
  Alcotest.(check (list string))
    "attached, POST parameters"
    [Eliom.Common.co_param_prefix ^ "v"; Eliom.Common.post_numstate_param_name]
    names;
  Alcotest.(check string)
    "attached, value" "x"
    (List.assoc (Eliom.Common.co_param_prefix ^ "v") post_params);
  let path, get_params, _, _, post_params = with_get in
  Alcotest.(check string) "path" "/p" path;
  Alcotest.(check (list (pair string string)))
    "GET parameters"
    ["g", "3"]
    get_params;
  Alcotest.(check (list (pair string string)))
    "POST parameters"
    ["x", "z"]
    post_params

let test_urls_needing_a_request () =
  let check msg f =
    match f () with
    | s -> Alcotest.failf "%s: %S" msg s
    | exception Eliom.Common.Request_information_not_available _ -> ()
  in
  Site.init ~app:"request" (fun () ->
    let s = get ["a"] Parameter.unit in
    let na =
      Service.create ~name:"na" ~path:Service.No_path
        ~meth:(Service.Get Parameter.unit) ()
    in
    Site.register na;
    check "relative URL" (fun () -> Uri.make_string_uri ~service:s ());
    check "non-attached coservice" (fun () ->
      Uri.make_string_uri ~absolute:true ~service:na ()))

let test_configuration () =
  let hostname, port, sslport, https =
    Site.init ~app:"configuration" (fun () ->
      ( Eliom.Config.get_default_hostname ()
      , Eliom.Config.get_default_port ()
      , Eliom.Config.get_default_sslport ()
      , Eliom.Config.default_protocol_is_https () ))
  in
  Alcotest.(check string) "hostname" Site.hostname hostname;
  Alcotest.(check int) "port" Site.http_port port;
  Alcotest.(check int) "HTTPS port" Site.https_port sslport;
  Alcotest.(check bool) "protocol" false https

let suite =
  ( "services"
  , [ Alcotest.test_case "outside a site" `Quick test_outside_a_site
    ; Alcotest.test_case "paths" `Quick test_paths
    ; Alcotest.test_case "path escaping" `Quick test_path_escaping
    ; Alcotest.test_case "site on several levels" `Quick test_site_levels
    ; Alcotest.test_case "default protocol" `Quick test_default_protocol
    ; Alcotest.test_case "standard ports" `Quick test_standard_ports
    ; Alcotest.test_case "absolute URLs" `Quick test_absolute_urls
    ; Alcotest.test_case "coservice URLs" `Quick test_coservice_urls
    ; Alcotest.test_case "anonymous coservices" `Quick test_anonymous_coservices
    ; Alcotest.test_case "POST URLs" `Quick test_post_urls
    ; Alcotest.test_case "URLs needing a request" `Quick
        test_urls_needing_a_request
    ; Alcotest.test_case "configuration" `Quick test_configuration ] )
