(* Please put in this file ONLY extensions of the standard OCaml library.
   And remove all the Eliom/Ocsigen specific stuff. *)


include (Eliom_client_common_base :
           module type of Eliom_client_common_base
         with type escaped_value = Eliom_lib_base.escaped_value
         with type +'a Client_value_server_repr.t =
           'a Eliom_lib_base.Client_value_server_repr.t
         with type client_value_datum = Eliom_lib_base.client_value_datum
         with type injection_datum = Eliom_lib_base.injection_datum
         with type compilation_unit_global_data =
           Eliom_lib_base.compilation_unit_global_data
         with type global_data := Eliom_lib_base.global_data
         with type request_data = Eliom_lib_base.request_data)

let escaped_value_escaped_value = fst

type +'a client_value = 'a Client_value_server_repr.t

let client_value_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int Eliom_lib_base.client_value_unwrap_id_int)

let create_client_value ?loc ~instance_id =
  Client_value_server_repr.create
    ?loc ~instance_id ~unwrapper:client_value_unwrapper
let client_value_from_server_repr cv = cv

let client_value_datum ~closure_id ~args ~value =
  {closure_id; args; value = Client_value_server_repr.to_poly value}

type +'a shared_value =
  {
    sh_server : 'a;
    sh_client : 'a client_value;
    sh_mark : 'a shared_value Eliom_wrap.wrapper
  }

let internal_wrap (x : 'a shared_value) : 'a client_value = x.sh_client

let shared_value_mark () : 'a shared_value Eliom_wrap.wrapper =
  Eliom_wrap.create_wrapper internal_wrap

let create_shared_value
    (v : 'a) (c : 'a client_value) : 'a shared_value =
  {sh_server = v;
   sh_client = c;
   sh_mark = shared_value_mark ()}

let shared_value_server_repr x = x.sh_server,x.sh_client

exception Client_value_creation_invalid_context of string

let escaped_value value : escaped_value (* * Eliom_wrap.unwrapper *) =
  to_poly value

type global_data = Eliom_lib_base.global_data * Eliom_wrap.unwrapper

let global_data_unwrapper =
  Eliom_wrap.create_unwrapper
    (Eliom_wrap.id_of_int global_data_unwrap_id_int)
