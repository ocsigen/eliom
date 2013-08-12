(* Ocsigen
 * Copyright (C) 2010 Simon Castellan
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

open Eliom_lib
open Eliom_s2s
open Ocsigen_stream
open Lwt
open Simplexmlparser
open Ocsigen_http_frame
open Cryptokit
open Eliom_parameter
open Eliom_content.Html5
open Ocsigen_headers
module Base64 = Netencoding.Base64

type openid_error =
  | Invalid_XRDS_File of string * string
  | Discovery_Error of string * string
  | Missing_parameter of string
  | Invalid_signature of string * string
  | Invalid_association of string
  | Invalid_argument of string * string * string
  | Server_error of string
  | Invalid_answer of string
  | Invalid_html_doc of string
let string_of_openid_error = function
  | Invalid_XRDS_File (url, error) ->
    Printf.sprintf "While parsing XRDS document at %s: %s" url error
  | Discovery_Error (url, error) ->
    Printf.sprintf "While fetching url %s: %s" url error
  | Missing_parameter name ->
    Printf.sprintf "Missing parameter: %s" name
  | Invalid_signature (ours, theirs) ->
    Printf.sprintf "Invalid signature: computed:'%s', got: '%s'" ours theirs
  | Invalid_association s ->
    "Invalid association with "^s
  | Invalid_argument (arg, v, value) ->
    Printf.sprintf "Invalid argument: %s. Got %s, expected %s" arg v value
  | Server_error s ->
    Printf.sprintf "Server error: %s" s
  | Invalid_answer s ->
    Printf.sprintf "Invalid server answer: %s" s
  | Invalid_html_doc url ->
    Printf.sprintf "Invalid HTML document (could not find endpoint): %s\n" url
exception Error of openid_error

let failwith e = raise (Error e)
let lwt_fail e = Lwt.fail (Error e)

(* COMMON *)
let ( &&& ) g1 g2 = g1 >>= (fun () -> g2)

let guard b e = if b then Lwt.return () else lwt_fail e

let get args name =
  try
    List.assoc name args
  with Not_found -> failwith (Missing_parameter name)
let check_arg args arg value =
  let v = get args arg in
  if v = value then Lwt.return ()
  else lwt_fail (Invalid_argument (arg, v, value))

let ( ^? ) a b = match a with
  | Some x -> x :: b
  | None -> b

let ( ^^? ) (b, x) l = if b then x :: l else l
let ( % ) a b = (a,  b)
let openid_url = "http://specs.openid.net/auth/2.0"
let direct_request ~mode ~params ~endpoint =
  let params = push_ns ~namespace_param:("ns", openid_url) "openid" (("mode", mode) :: params) in
  direct_request params endpoint

(* DISCOVERY *)
(* This part is dedicated to discover the OpenID service attached
   to a given endpoint. To do so, we do a GET request towards a remote file
   that may be in two forms :
   - An XRDS file describing a list of service. For now we take the one with the
   higher priority.
   - An html file in which we have to seek for two <link /> tags.
*)
(* XRDS parsing *)
let parse_xrds url xml =
  let parse_item (endpoint, local) = function
    | Element ("URI", _, [PCData uri]) ->
      (uri, local)
    | Element ("LocalID", _, [PCData id]) ->
      (endpoint, Some id)
    | _ -> (endpoint, local)
  in
  let parse_service = function
    | Element ("Service", attrs, children) ->
	let priority =
	  try
	    snd (List.find (function ("priority", _) -> true | _ -> false) attrs)
	  with Not_found -> failwith (Invalid_XRDS_File (url, "Priority expected"))
	in
	  (int_of_string priority, List.fold_left parse_item ("", None) children)
    | _ -> failwith (Invalid_XRDS_File (url, "Service expected"))
  in
    match xml with
      | [Element ("xrds:XRDS", _, [Element ("XRD", _, services)])] ->
	  List.map parse_service services
      | _ ->
	  failwith (Invalid_XRDS_File (url, "Wrong root XML element"))
(* Select the best service *)
let select_service services = List.fold_left
  (fun ((a, _) as data) ((b, _) as data') -> if b < a then data' else data)
  (List.hd services)
  (List.tl services)

(* HTML Parsing *)
(* About this function:
   We cannot afford putting the whole page in memory and parsing
   it, and then looking for some <link> tags. Some pages are actually
   too big for that. So we parse chunk by chunk the stream given by
   Ocsigen_http_client, seeking for some particular regexp.
   Where the fun part goes is that some providers don't bother using valid XML, so
   we have something like <link rel="stylesheet" href="style.css"> (myopenid.net).
   Fortunately for the link we're interested in, they seem to respect the standard
   so we ignore the ones where Simplexmlparser fails, crossing the fingers. *)
let parse_html url stream =
  let server = ref "" and delegate = ref (Some url) in
  let rec loop acc st = Ocsigen_stream.next st >>=
    (function
      | Ocsigen_stream.Finished (Some st) -> loop acc st
      | Ocsigen_stream.Finished None ->
        if !server = "" then
          lwt_fail (Invalid_html_doc url)
        else
          Lwt.return (!server, !delegate)
      | Ocsigen_stream.Cont (suite, st) ->
        compute_match (acc ^ suite) st)
  and compute_match acc st =
    let regexp = Netstring_pcre.regexp "< *link[^>]*>" in
    let next acc = loop acc st in
    try
      let (k, r) = Netstring_pcre.search_forward regexp acc 0 in
      let m = Netstring_pcre.matched_string r acc in
      (try
         match Simplexmlparser.xmlparser_string m with
           | Element ("link", attrs, _) :: _ ->
             if List.mem ("rel", "openid.server") attrs then
               server := List.assoc "href" attrs
             else if List.mem ("rel", "openid.delegate") attrs then
               delegate := Some (List.assoc "href" attrs)
           | _ -> ()
       with _ -> ());
      let s' = String.sub acc (k + String.length m) (String.length acc - String.length m - k) in
      compute_match s' st
    with Not_found ->
      let k = String.rindex acc '<' in
      next (String.sub acc k (String.length acc - k))

  in
  loop "" stream

(* Request end-point *)
(* DO NOT HANDLE XRIs *)
let normalize url =
  let starts_with s =
    String.length url > String.length s
    && String.sub url 0 (String.length s) = s in
  if not (starts_with "http://") && not (starts_with "https://") then
    "http://" ^ url
  else
    url
let perform_discovery url =
  let url = normalize url in
  do_get_request url >>= (fun frame ->
    match frame.frame_content with
      | None -> failwith (Discovery_Error (url, "Empty body"))
      | Some content ->
        let content = Ocsigen_stream.get content in
        match Ocsigen_headers.parse_content_type (get_content_type frame) with
          | Some ((_, "xrds+xml"), _) ->
            string_of_stream 100000 content >>= (fun s ->
        let xml = Simplexmlparser.xmlparser_string s in
            let _, service = select_service (parse_xrds url xml) in
            Lwt.return service)
          | _ -> parse_html url content)


(* CRYPT *)

(* this is default modulus and generator given by the openid spec. *)
let modulus = "\xDC\xF9\x3A\x0B\x88\x39\x72\xEC\x0E\x19\x98\x9A\xC5\xA2\xCE\x31\x0E\x1D\x37\x71\x7E\x8D\x95\x71\xBB\x76\x23\x73\x18\x66\xE6\x1E\xF7\x5A\x2E\x27\x89\x8B\x05\x7F\x98\x91\xC2\xE2\x7A\x63\x9C\x3F\x29\xB6\x08\x14\x58\x1C\xD3\xB2\xCA\x39\x86\xD2\x68\x37\x05\x57\x7D\x45\xC2\xE7\xE5\x2D\xC8\x1C\x7A\x17\x18\x76\xE5\xCE\xA7\x4B\x14\x48\xBF\xDF\xAF\x18\x82\x8E\xFD\x25\x19\xF1\x4E\x45\xE3\x82\x66\x34\xAF\x19\x49\xE5\xB5\x35\xCC\x82\x9A\x48\x3B\x8A\x76\x22\x3E\x5D\x49\x0A\x25\x7F\x05\xBD\xFF\x16\xF2\xFB\x22\xC5\x83\xAB"
let g = "\x02"
let openid_param = { DH.p = modulus; DH.g = g; DH.privlen = 160 }

let unpad s =
  let rec loop acc k =
    if k = String.length s - 1 then acc
    else if s.[k] = '\000' && (s.[k+1] = '\000' || int_of_char s.[k+1] land 128 = 0) then
      loop (acc + 1) (k + 1)
    else
      acc
  in
  let off = loop 0 0 in
  String.sub s off (String.length s - off)
let encode_number message =
  unpad (if int_of_char message.[0] land 128 <> 0 then
    "\x00" ^ message
  else message)

let output_secret s =
  let message = DH.message openid_param s in
  Base64.encode (encode_number message)

let gen_new_secret () =
  DH.private_secret openid_param

let gen_signature ~key ~args =
  let kw = String.concat ""
    (List.map (fun a -> Printf.sprintf "%s:%s\n" a (get args a))
       (Netstring_pcre.split (Netstring_pcre.regexp ",") (get args "signed")))
  in
  hash_string (MAC.hmac_sha1 key) kw

let check_signature ~key ~args =
  let sig1 = Base64.encode (gen_signature ~key ~args)
  and sig2 = get args "sig"
  in
  if sig1 <> sig2 then lwt_fail (Invalid_signature (sig1, sig2))
  else Lwt.return ()

let decode_number s =
  if '\x00' = s.[0] then
    String.sub s 1 (String.length s - 1)
  else s

let decrypt ~encrypted_mac ~secret ~pub_server =
  let pub_server = decode_number pub_server in
  let shared_secret = DH.shared_secret openid_param secret pub_server in
  let shared_secret = unpad shared_secret in
  let hzz = hash_string (Hash.sha1 ()) shared_secret in
  let mac = String.make (String.length hzz) ' ' in
  for k = 0 to String.length mac - 1 do
    mac.[k] <- char_of_int ((int_of_char hzz.[k]) lxor (int_of_char encrypted_mac.[k]))
  done;
  mac


(* ASSOCIATION *)
(* This library only implements the stateful mode :
   we first associate to the remote endpoint and store
   the cryptographic data to be able to check the future signature ourselves *)
type assoctmp = {
  t_mac : string;
  t_assoc_handle : string;
  t_delay : float;
  t_secret : Cryptokit.DH.private_secret;
  t_mac_crypted : string;
  t_server_public : string;
}

type assoc = {
  mac : string; (* The crypting key *)
  assoc_handle : string; (* The handle used by the server to remember us. *)
  delay : float; (* The time by which the association will have expired *)
}
module M = Map.Make (struct type t = string let compare = compare end)
let associations = ref M.empty

let associate endpoint =
  let secret = gen_new_secret () in
  let parse args =
    let rec aux v (name, value) =
      match name with
        | "expires_in" -> { v with t_delay = float_of_string value +. Unix.time () }
        | "enc_mac_key" -> { v with t_mac_crypted = Base64.decode value }
        | "mac_key" -> { v with t_mac = Base64.decode value }
        | "dh_server_public" -> { v with t_server_public = Base64.decode value }
        | "assoc_handle" -> { v with t_assoc_handle = value }
        | _ -> v
    in
    let assoc = List.fold_left aux { t_mac = ""; t_mac_crypted = "";
                                     t_assoc_handle = ""; t_delay = 0.;
                                     t_secret = secret;
                                     t_server_public = ""; } args in
    if assoc.t_mac = "" &&
       (assoc.t_mac_crypted = "" || assoc.t_server_public = "") then
      lwt_fail (Invalid_association endpoint)
    else begin
      let assoc = {
        delay = assoc.t_delay;
        mac =
          (if assoc.t_mac <> "" then assoc.t_mac
           else decrypt ~encrypted_mac:assoc.t_mac_crypted ~secret
              ~pub_server: assoc.t_server_public);
        assoc_handle = assoc.t_assoc_handle
      }
      in
      Ocsigen_messages.warning (Printf.sprintf "--OpenID: Associated to `%s' (%s)"
                                  endpoint (Base64.encode assoc.mac));
      Lwt.return assoc
    end
  in
  direct_request
    ~mode:"associate"
    ~params: ["assoc_type", "HMAC-SHA1";
              "session_type", "DH-SHA1";
              "dh_consumer_public", output_secret secret]
    ~endpoint >>= parse

let get_assoc end_point =
  try
    return (M.find end_point !associations)
  with Not_found ->
    associate end_point >>= (fun v ->
      associations := M.add end_point v !associations;
    return v)


let reassociate end_point =
  Ocsigen_messages.warning ("OpenID: reassociating to " ^ end_point);
  associations := M.remove end_point !associations;
  get_assoc end_point


(* CHECK *)
let scope = `Session (Eliom_common.create_scope_hierarchy "__eliom_openid")
let group_name = "__eliom_openid_group"

type field =
  | Email
  | Fullname
  | DateOfBirth
  | PostCode
  | Timezone
  | Language
  | Country
  | Gender
  | Nickname

let field_names = [
  Email, "email";
  Fullname, "fullname";
  Nickname, "nickname";
  DateOfBirth, "dob";
  Gender, "gender";
  PostCode, "postcode";
  Country, "country";
  Language, "language";
  Timezone, "timezone";
]
let field_names_rev = List.map (fun (x, y) -> (y, x)) field_names

(* DEALING WITH EXTENSIONS *)
type 'b extension = {
  headers: (string * string) list;
  parse: (string * string) list -> 'b Lwt.t
}

(* sreg extension
   See http://openid.net/specs/openid-simple-registration-extension-1_0.html *)
let format_demands ~required ~required_name ~optional ~optional_name =
  let get y = List.assoc y field_names in
  let fmt l = String.concat "," (List.map get l) in
  ((required <> []) % (required_name, fmt required)) ^^?
    ((optional <> []) % (optional_name, fmt optional)) ^^? []

let sreg ?policy_url ~required ~optional () =
  let li = format_demands ~required ~optional
    ~required_name: "required" ~optional_name: "optional"
    @ (Option.map (fun x -> "policy_url", x) policy_url ^? [])
  in
  let sreg_url = "http://openid.net/extensions/sreg/1.1" in
  {
    headers = ("ns.sreg", sreg_url) :: push_ns "sreg" li;
    parse = (fun args ->
      let args = find_in_ns ~default_namespace: "sreg"  sreg_url args in
      let args = List.map (fun (x, y) -> List.assoc x field_names_rev, y) args in
      Lwt.return args)
  }

(* ax extension *)
let urls =
  [     "type.email","http://axschema.org/contact/email";
        "type.nickname", "http://axschema.org/namePerson/friendly";
        "type.fullname", "http://axschema.org/namePerson";
        "type.dob", "http://axschema.org/birthDate";
        "type.gender", "http://axschema.org/person/gender";
        "type.postcode", "http://axschema.org/contact/postalCode/home";
        "type.country", "http://axschema.org/contact/country/home";
        "type.language", "http://axschema.org/pref/language";
        "type.timezone","http://axschema.org/pref/timezone" ]


let ax ~required ~optional () =
  let url = "http://openid.net/srv/ax/1.0" in
  let li =
    let fields = required @ optional in
    List.map (fun info ->
      let name = "type."^List.assoc info field_names in
      (name, List.assoc name urls)) fields
    @
      format_demands ~required ~required_name:"required"
      ~optional ~optional_name: "if_available"
  in
  { headers = ("ns.ax", url) :: push_ns "ax" (("mode", "fetch_request") :: li);
    parse =  (fun args ->
      let args = find_in_ns ~default_namespace: "ax" url args in
      let args = strip_ns "value" args in
      let args = List.map (fun (x, y) -> List.assoc x field_names_rev, y) args in
      Lwt.return args)
  }

(* PAPE *)
type pape = { auth_time : string option; policies : string list option; nist_level : int option }
let build_opt_list list =
  List.fold_left (fun li -> function
    | a, Some x -> (a, x) :: li
    | _, None -> li) [] list

let assoc_opt a l = try Some (List.assoc a l) with Not_found -> None
let pape ?max_auth_age ?auth_policies () =
  let url = "http://specs.openid.net/extensions/pape/1.0" in
  {
    headers =
      (build_opt_list
        ["ns.pape", Some url;
         "pape.max_auth_age", Option.map string_of_int max_auth_age;
         "pape.preferred_auth_policies", Option.map (String.concat ",") auth_policies]);
    parse = (fun args ->
      let args = find_in_ns ~default_namespace: "pape" url args in
      let auth_time = assoc_opt "auth_time" args in
      let policies = Option.map (String.split ',') (assoc_opt "auth_policies" args) in
      let nist_level = Option.map int_of_string (assoc_opt "nist_auth_level" args) in
      Lwt.return
        { auth_time = auth_time; policies = policies;
          nist_level = nist_level })
  }

let ( *** ) e1 e2 = {
  headers = e1.headers @ e2.headers;
  parse = (fun l -> e1.parse l >>= (fun a -> e2.parse l >>= (fun b -> return (a, b))))
}

(* CHECKING *)
let check_authentication ret_to endpoint assoc args =
  check_arg args "return_to" ret_to
  &&& check_signature assoc.mac args

type 'a authentication_result =
  | Canceled
  | Setup_needed
  | Result of 'a

let end_login_handler ext ret_to endpoint assoc f args =
  let args = strip_ns "openid" args in
  let mode = get args "mode" in
  let _ = Eliom_state.discard ~scope () in
  if mode = "id_res" then
    (if List.mem_assoc "invalidate_handle" args then
        reassociate endpoint
     else
        Lwt.return assoc) >>=
      (fun assoc -> check_authentication ret_to endpoint assoc args) >>=
      (fun () -> ext.parse args) >>= (fun k -> f (Result k))
  else if mode = "cancel" then
    f Canceled
  else if mode = "setup_needed" then
    f Setup_needed
  else if mode = "error" then
    lwt_fail (Server_error (get args "error"))
  else
    lwt_fail (Invalid_answer mode)

module type HiddenServiceInfo = sig
  val path : string list
(** The path of the hidden service *)
  val f :
    (string * string) list ->
    unit -> Eliom_registration.browser_content Eliom_registration.kind Lwt.t
(** The function called when an user connects to the hidden service
    (not that hidden) without being in an identification process.
    Typically you should redirect the user to the login page. *)
end

module Make (S : HiddenServiceInfo) = struct
  let return_service = Eliom_service.Unsafe.service ~path:S.path ~get_params:any ()

  let () = Eliom_registration.Any.register ~service:return_service S.f

  let authenticate ~mode ~ext ~handler ~discovery =
    let local = match snd discovery with
      | None ->  "http://specs.openid.net/auth/2.0/identifier_select"
      | Some l -> l
    in
    get_assoc (fst discovery) >>= fun assoc ->
    let uri = ref "" in
    let () = uri :=
      Eliom_content.Html5.F.make_string_uri ~absolute: true
      ~service:return_service []
    in
    let _ = Eliom_registration.Any.register
      ~scope
      ~service:return_service
      (fun args _ ->
        end_login_handler ext !uri (fst discovery) assoc handler args)
    in
    let _ = Eliom_state.set_service_session_group
      ~set_max: 1000
      ~scope
      group_name
    in
    let _ = Eliom_state.set_global_service_state_timeout
      ~cookie_scope:scope
      (Some 60.)
    in
    let params =
      ["return_to", !uri;
       "claimed_id", local;
       "identity", local;
       "assoc_handle", assoc.assoc_handle;
       "realm", "http://"^Eliom_request_info.get_hostname ()] @ ext.headers
    in
    let params = push_ns "openid" (("ns", openid_url) :: ("mode", mode) :: params) in
    Lwt.return (format_url (fst discovery) params)
end

(* GLUE *)
type result = {
  fields : (field * string) list;
  pape : pape;
}

let dispatch f = function
  | Result (l, (l', pape)) ->
    let r = { fields = l @ l'; pape = pape } in
    f (Result r)
  | (Canceled | Setup_needed) as x -> f x


let check = ref (fun ?mode ~ext ~handler ~discovery -> Lwt.fail (Failure "Call OpenID.init"))

type check_fun =
    ?immediate:bool ->
    ?policy_url:string ->
    ?max_auth_age:int ->
    ?auth_policies:string list ->
    ?required:field list ->
    ?optional:field list ->
    string ->
    (result authentication_result ->
     Eliom_registration.browser_content Eliom_registration.kind Lwt.t) ->
    Url.t Lwt.t

let check check ?(immediate = true) ?policy_url ?max_auth_age ?auth_policies
    ?(required = []) ?(optional = []) user_url handler =
  let mode =
    if immediate then "checkid_immediate"
    else "checkid_setup"
  in
  perform_discovery user_url >>= (fun discovery ->
    check
      ~mode
      ~ext: (ax ~required ~optional () ***
             sreg ?policy_url ~required ~optional () ***
             pape ?max_auth_age ?auth_policies ())
      ~handler: (dispatch handler)
      ~discovery)

let init ~path ~f =
  let module K = Make (struct let path = path let f = f end) in
  check K.authenticate
