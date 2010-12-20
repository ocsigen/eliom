type sitedata =
  {site_dir: Ocsigen_lib.url_path;
   site_dir_string: string;
  }

type server_params

val sp : server_params

type 'a data_key

val to_data_key_ : (int64 * int) -> 'a data_key
val of_data_key_ : 'a data_key -> (int64 * int)

(* Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal : 'a -> string
val string_escape : string -> string


(**/**)
type eliom_data_type =
    ((XML.ref_tree, (int * XML.ref_tree) list) Ocsigen_lib.leftright *
        ((int64 * int) * unit list) *
        Ocsigen_cookies.cookieset *
        string list (* on load scripts *) *
        string list (* on change scripts *) *
        Eliom_common.sess_info
    )

type eliom_appl_answer =
  | EAContent of (eliom_data_type * string)

val a_closure_id : int
val a_closure_id_string : string
val add_tab_cookies_to_get_form_id : int
val add_tab_cookies_to_get_form_id_string : string
val add_tab_cookies_to_post_form_id : int
val add_tab_cookies_to_post_form_id_string : string

val eliom_appl_answer_content_type : string
val encode_eliom_data : 'a -> string

val string_escape : string -> string
