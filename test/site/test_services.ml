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
    ; Alcotest.test_case "absolute URLs" `Quick test_absolute_urls
    ; Alcotest.test_case "coservice URLs" `Quick test_coservice_urls
    ; Alcotest.test_case "URLs needing a request" `Quick
        test_urls_needing_a_request
    ; Alcotest.test_case "configuration" `Quick test_configuration ] )
