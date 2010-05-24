type sitedata =
  {site_dir: Ocsigen_lib.url_path;
   site_dir_string: string;
  }


type server_params =
    {
     sp_si: Eliom_common.sess_info;
     sp_sitedata: sitedata (* data for the whole site *);
(*     sp_cookie_info: tables cookie_info; *)
     sp_suffix: Ocsigen_lib.url_path option (* suffix *);
     sp_fullsessname: string option (* the name of the session
                                       to which belong the service
                                       that answered
                                       (if it is a session service) *)}


type 'a data_key

val to_data_key_ : (int * int) -> 'a data_key
val of_data_key_ : 'a data_key -> (int * int)

(* Marshal an OCaml value into a string. All characters are escaped *)
val jsmarshal : 'a -> string


(**/**)
val a_closure_id : int
val a_closure_id_string : string


