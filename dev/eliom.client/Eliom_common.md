# Module `Eliom_common`

```ocaml
exception Eliom_site_information_not_available of string
```
```ocaml
type scope_hierarchy = Eliom_common_base.scope_hierarchy = 
  | User_hier of string
  | Default_ref_hier
  | Default_comet_hier
```
```ocaml
type user_scope = [ 
  | `Session_group of scope_hierarchy
  | `Session of scope_hierarchy
  | `Client_process of scope_hierarchy
 ]
```
```ocaml
type scope = [ 
  | `Site
  | user_scope
 ]
```
```ocaml
type all_scope = [ 
  | scope
  | `Global
  | `Request
 ]
```
```ocaml
type global_scope = [ 
  | `Global
 ]
```
```ocaml
type site_scope = [ 
  | `Site
 ]
```
```ocaml
type session_group_scope = [ 
  | `Session_group of scope_hierarchy
 ]
```
```ocaml
type session_scope = [ 
  | `Session of scope_hierarchy
 ]
```
```ocaml
type client_process_scope = [ 
  | `Client_process of scope_hierarchy
 ]
```
```ocaml
type request_scope = [ 
  | `Request
 ]
```
```ocaml
type user_level = [ 
  | `Session_group
  | `Session
  | `Client_process
 ]
```
```ocaml
type cookie_level = [ 
  | `Session
  | `Client_process
 ]
```
```ocaml
type cookie_scope = [ 
  | `Session of scope_hierarchy
  | `Client_process of scope_hierarchy
 ]
```
```ocaml
val level_of_user_scope : 
  [< `Client_process of scope_hierarchy & 'a
  | `Session of scope_hierarchy & 'b
  | `Session_group of scope_hierarchy & 'c ] ->
  [> user_level ]
```
```ocaml
val cookie_level_of_user_scope : 
  [< `Client_process of scope_hierarchy & 'a
  | `Session of scope_hierarchy & 'b
  | `Session_group of scope_hierarchy & 'c ] ->
  [> cookie_level ]
```
```ocaml
val cookie_scope_of_user_scope : [< user_scope ] -> [> cookie_scope ]
```
```ocaml
val scope_hierarchy_of_user_scope : [< user_scope ] -> scope_hierarchy
```
```ocaml
type full_state_name = Eliom_common_base.full_state_name = {
  user_scope : user_scope;
  secure : bool;
  site_dir_str : string;
}
```
```ocaml
module Full_state_name_table = Eliom_common_base.Full_state_name_table
```
```ocaml
type att_key_serv = Eliom_common_base.att_key_serv = 
  | SAtt_no
  | SAtt_named of string
  | SAtt_anon of string
  | SAtt_csrf_safe of int * user_scope * bool option
  | SAtt_na_named of string
  | SAtt_na_anon of string
  | SAtt_na_csrf_safe of int * user_scope * bool option
```
```ocaml
type na_key_serv = Eliom_common_base.na_key_serv = 
  | SNa_no
  | SNa_void_keep
  | SNa_void_dontkeep
  | SNa_get_ of string
  | SNa_post_ of string
  | SNa_get' of string
  | SNa_post' of string
  | SNa_get_csrf_safe of int * user_scope * bool option
  | SNa_post_csrf_safe of int * user_scope * bool option
```
```ocaml
type att_key_req = Eliom_common_base.att_key_req = 
  | RAtt_no
  | RAtt_named of string
  | RAtt_anon of string
```
```ocaml
type na_key_req = Eliom_common_base.na_key_req = 
  | RNa_no
  | RNa_get_ of string
  | RNa_post_ of string
  | RNa_get' of string
  | RNa_post' of string
```
```ocaml
val att_key_serv_of_req : att_key_req -> att_key_serv
```
```ocaml
val na_key_serv_of_req : na_key_req -> na_key_serv
```
```ocaml
val defaultpagename : string
```
```ocaml
val eliom_suffix_name : string
```
```ocaml
val eliom_suffix_internal_name : string
```
```ocaml
val eliom_nosuffix_page : string
```
```ocaml
val naservice_num : string
```
```ocaml
val naservice_name : string
```
```ocaml
val get_state_param_name : string
```
```ocaml
val post_state_param_name : string
```
```ocaml
val get_numstate_param_name : string
```
```ocaml
val post_numstate_param_name : string
```
```ocaml
val co_param_prefix : string
```
```ocaml
val na_co_param_prefix : string
```
```ocaml
val nl_param_prefix : string
```
```ocaml
val pnl_param_prefix : string
```
```ocaml
val npnl_param_prefix : string
```
```ocaml
val eliom_internal_nlp_prefix : string
```
```ocaml
val tab_cookies_param_name : string
```
```ocaml
val to_be_considered_as_get_param_name : string
```
```ocaml
val appl_name_cookie_name : string
```
```ocaml
val appl_name_header_name : string
```
```ocaml
val full_xhr_redir_header : string
```
```ocaml
val half_xhr_redir_header : string
```
```ocaml
val response_url_header : string
```
```ocaml
val set_tab_cookies_header_name : string
```
```ocaml
val tab_cookies_header_name : string
```
```ocaml
val cookie_substitutes_header_name : string
```
```ocaml
val set_cookie_substitutes_header_name : string
```
```ocaml
val tab_cpi_header_name : string
```
```ocaml
val expecting_process_page_name : string
```
```ocaml
val base_elt_id : string
```
```ocaml
val nl_is_persistent : string -> bool
```
```ocaml
type client_process_info = Eliom_common_base.client_process_info = {
  cpi_ssl : bool;
  cpi_hostname : string;
  cpi_server_port : int;
  cpi_original_full_path : string list;
}
```
```ocaml
val client_process_info_of_json : 
  Deriving_Json_lexer.lexbuf ->
  client_process_info
```
```ocaml
val client_process_info_to_json : Buffer.t -> client_process_info -> unit
```
```ocaml
val client_process_info_json : client_process_info Deriving_Json.t
```
```ocaml
type sess_info = Eliom_common_base.sess_info = {
  si_other_get_params : (string * string) list;
  si_all_get_params : (string * string) list;
  si_all_post_params : (string * string) list option;
  si_all_file_params : (string * Eliom_lib.file_info) list option;
  si_service_session_cookies : string Full_state_name_table.t;
  si_data_session_cookies : string Full_state_name_table.t;
  si_persistent_session_cookies : string Full_state_name_table.t;
  si_secure_cookie_info : string Full_state_name_table.t
                        * string Full_state_name_table.t
                        * string Full_state_name_table.t;
  si_service_session_cookies_tab : string Full_state_name_table.t;
  si_data_session_cookies_tab : string Full_state_name_table.t;
  si_persistent_session_cookies_tab : string Full_state_name_table.t;
  si_secure_cookie_info_tab : string Full_state_name_table.t
                            * string Full_state_name_table.t
                            * string Full_state_name_table.t;
  si_tab_cookies : string Ocsigen_cookie_map.Map_inner.t;
  si_nonatt_info : na_key_req;
  si_state_info : att_key_req * att_key_req;
  si_previous_extension_error : int;
  si_na_get_params : (string * string) list Lazy.t;
  si_nl_get_params : (string * string) list Eliom_lib.String.Table.t;
  si_nl_post_params : (string * string) list Eliom_lib.String.Table.t;
  si_nl_file_params : (string * Eliom_lib.file_info) list
                      Eliom_lib.String.Table.t;
  si_persistent_nl_get_params : (string * string) list Eliom_lib.String.Table.t
                                Lazy.t;
  si_all_get_but_na_nl : (string * string) list Lazy.t;
  si_all_get_but_nl : (string * string) list;
  si_ignored_get_params : (string * string) list;
  si_ignored_post_params : (string * string) list;
  si_client_process_info : client_process_info option;
  si_expect_process_data : bool Lazy.t;
}
```
```ocaml
type eliom_js_page_data = Eliom_common_base.eliom_js_page_data = {
  ejs_global_data : (Eliom_runtime.global_data * Eliom_wrap.unwrapper) option;
  ejs_request_data : Eliom_runtime.request_data;
  ejs_event_handler_table : Ocsigen_lib_base.poly
                            Eliom_runtime.RawXML.ClosureMap.t;
  ejs_client_attrib_table : Ocsigen_lib_base.poly
                            Eliom_runtime.RawXML.ClosureMap.t;
  ejs_sess_info : sess_info;
}
```
```ocaml
val tyxml_unwrap_id_int : int
```
```ocaml
val comet_channel_unwrap_id_int : int
```
```ocaml
val react_up_unwrap_id_int : int
```
```ocaml
val react_down_unwrap_id_int : int
```
```ocaml
val signal_down_unwrap_id_int : int
```
```ocaml
val bus_unwrap_id_int : int
```
```ocaml
val client_value_unwrap_id_int : int
```
```ocaml
val global_data_unwrap_id_int : int
```
```ocaml
val server_function_unwrap_id_int : int
```
```ocaml
type node_ref = string
```
```ocaml
val nl_get_appl_parameter : string
```
```ocaml
val make_actual_path : string list -> string list
```
```ocaml
val is_client_app : bool ref
```
```ocaml
val prefixlength : int
```
```ocaml
val prefixlengthminusone : int
```
```ocaml
val split_nl_prefix_param : 
  (string * 'a) list ->
  (string * 'a) list Eliom_lib.String.Table.t * (string * 'a) list
```
```ocaml
val split_prefix_param : 
  string ->
  (string * 'a) list ->
  (string * 'a) list * (string * 'a) list
```
```ocaml
val remove_prefixed_param : string -> (string * 'a) list -> (string * 'a) list
```
```ocaml
val remove_na_prefix_params : (string * 'a) list -> (string * 'a) list
```
```ocaml
val filter_na_get_params : (string * string) list -> (string * string) list
```
```ocaml
exception Eliom_404
```
```ocaml
type ('a, 'b) foundornot = ('a, 'b) Eliom_common_base.foundornot = 
  | Found of 'a
  | Notfound of 'b
```
Service called with wrong parameter names

```ocaml
exception Eliom_Wrong_parameter
```
```ocaml
exception Eliom_duplicate_registration of string
```
```ocaml
exception Eliom_page_erasing of string
```
```ocaml
type 'a dircontent = 'a Eliom_common_base.dircontent = 
  | Vide
  | Table of 'a direlt ref Eliom_lib.String.Table.t
```
```ocaml
and 'a direlt = 'a Eliom_common_base.direlt = 
  | Dir of 'a dircontent ref
  | File of 'a ref
```
```ocaml
val empty_dircontent : unit -> 'a dircontent
```
```ocaml
type meth = [ 
  | `Get
  | `Post
  | `Put
  | `Delete
  | `Other
 ]
```
```ocaml
type page_table_key = Eliom_common_base.page_table_key = {
  key_state : att_key_serv * att_key_serv;
  key_meth : meth;
}
```
```ocaml
type anon_params_type = int
```
```ocaml
exception Eliom_Typing_Error of (string * exn) list
```
```ocaml
type ('params, 'result) service = ('params, 'result) Eliom_common_base.service =
  {
  s_id : anon_params_type * anon_params_type;
  mutable s_max_use : int option;
  s_expire : (float * float ref) option;
  s_f : bool -> 'params -> 'result Lwt.t;
}
```
```ocaml
type 'a to_and_of = 'a Eliom_common_base.to_and_of = {
  of_string : string -> 'a;
  to_string : 'a -> string;
}
```
```ocaml
val backtrace_lwt : int -> string list
```
```ocaml
type server_params = unit
```
```ocaml
val get_sp : unit -> unit
```
```ocaml
val get_sp_option : unit -> unit option
```
```ocaml
type 'a wrapper = unit
```
```ocaml
val make_wrapper : 'b -> 'a wrapper
```
```ocaml
val empty_wrapper : unit -> 'a wrapper
```
```ocaml
type unwrap_id = Eliom_unwrap.unwrap_id
```
```ocaml
val react_up_unwrap_id : unwrap_id
```
```ocaml
val react_down_unwrap_id : unwrap_id
```
```ocaml
val signal_down_unwrap_id : unwrap_id
```
```ocaml
val comet_channel_unwrap_id : unwrap_id
```
```ocaml
val bus_unwrap_id : unwrap_id
```
```ocaml
val sitedata : Eliom_types.sitedata option ref
```
```ocaml
val global_register_allowed : unit -> (unit -> Eliom_types.sitedata) option
```
```ocaml
val get_site_dir : Eliom_types.sitedata -> Eliom_lib.Url.path
```
```ocaml
val get_site_dir_string : Eliom_types.sitedata -> string
```
```ocaml
val add_unregistered : 'a -> 'b -> unit
```
```ocaml
module To_and_of_shared : sig ... end
```
```ocaml
val client_html_file : unit -> string
```
```ocaml
val set_client_html_file : string -> unit
```
```ocaml
val defer : (unit -> 'a option) -> ('a -> 'b) -> 'b option ref
```
