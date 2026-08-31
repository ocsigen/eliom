# Module `Eliom_comet_base`

```ocaml
type 'a chan_id
```
```ocaml
val string_of_chan_id : 'a chan_id -> string
```
```ocaml
val chan_id_of_string : string -> 'a chan_id
```
```ocaml
type position = 
  | Newest of int
  | After of int
  | Last of int option (* None means 'newest channel' *)
```
```ocaml
val position_of_json : Deriving_Json_lexer.lexbuf -> position
```
```ocaml
val position_to_json : Buffer.t -> position -> unit
```
```ocaml
val position_json : position Deriving_Json.t
```
```ocaml
type comet_stateless_request = (string * position) array
```
```ocaml
val comet_stateless_request_of_json : 
  Deriving_Json_lexer.lexbuf ->
  comet_stateless_request
```
```ocaml
val comet_stateless_request_to_json : 
  Buffer.t ->
  comet_stateless_request ->
  unit
```
```ocaml
val comet_stateless_request_json : comet_stateless_request Deriving_Json.t
```
```ocaml
type command = 
  | Register of string
  | Close of string
```
```ocaml
val command_of_json : Deriving_Json_lexer.lexbuf -> command
```
```ocaml
val command_to_json : Buffer.t -> command -> unit
```
```ocaml
val command_json : command Deriving_Json.t
```
```ocaml
type comet_stateful_request = 
  | Request_data of int
  | Commands of command array
```
```ocaml
val comet_stateful_request_of_json : 
  Deriving_Json_lexer.lexbuf ->
  comet_stateful_request
```
```ocaml
val comet_stateful_request_to_json : Buffer.t -> comet_stateful_request -> unit
```
```ocaml
val comet_stateful_request_json : comet_stateful_request Deriving_Json.t
```
```ocaml
type comet_request = 
  | Stateless of comet_stateless_request
  | Stateful of comet_stateful_request
```
```ocaml
val comet_request_of_json : Deriving_Json_lexer.lexbuf -> comet_request
```
```ocaml
val comet_request_to_json : Buffer.t -> comet_request -> unit
```
```ocaml
val comet_request_json : comet_request Deriving_Json.t
```
```ocaml
val comet_request_param : 
  (comet_request,
    [ `WithoutSuffix ],
    [ `One of comet_request Eliom_parameter.ocaml ] Eliom_parameter.param_name)
    Eliom_parameter.params_type
```
```ocaml
type 'a channel_data = 
  | Data of 'a
  | Full
  | Closed
```
```ocaml
val channel_data_of_json : 
  (Deriving_Json_lexer.lexbuf -> 'a) ->
  Deriving_Json_lexer.lexbuf ->
  'a channel_data
```
```ocaml
val channel_data_to_json : 
  (Buffer.t -> 'a -> unit) ->
  Buffer.t ->
  'a channel_data ->
  unit
```
```ocaml
val channel_data_json : 'a Deriving_Json.t -> 'a channel_data Deriving_Json.t
```
```ocaml
type answer = 
  | Stateless_messages of (string * (string * int) channel_data) array
  | Stateful_messages of (string * string channel_data) array
  | Timeout
  | State_closed
  | Comet_error of string
```
```ocaml
val answer_of_json : Deriving_Json_lexer.lexbuf -> answer
```
```ocaml
val answer_to_json : Buffer.t -> answer -> unit
```
```ocaml
val answer_json : answer Deriving_Json.t
```
```ocaml
type comet_service = 
  | Comet_service : (unit,
                    bool * comet_request,
                    Eliom_service.post,
                    Eliom_service.att,
                    _,
                    _,
                    _,
                    [ `WithoutSuffix ],
                    unit,
                    [ `One of bool ] Eliom_parameter.param_name
                    * [ `One of comet_request Eliom_parameter.ocaml ]
                        Eliom_parameter.param_name,
                    Eliom_service.non_ocaml)
                    Eliom_service.t
    * command list ref -> comet_service
```
```ocaml
type internal_comet_service = 
  | Internal_comet_service : (unit,
                             bool * comet_request,
                             Eliom_service.post,
                             Eliom_service.att,
                             _,
                             Eliom_service.non_ext,
                             Eliom_service.reg,
                             [ `WithoutSuffix ],
                             unit,
                             [ `One of bool ] Eliom_parameter.param_name
                             * [ `One of comet_request Eliom_parameter.ocaml ]
                                 Eliom_parameter.param_name,
                             Eliom_service.non_ocaml)
                             Eliom_service.t
    * command list ref -> internal_comet_service
```
```ocaml
type stateless_kind = 
  | After_kind of int
  | Newest_kind of int
  | Last_kind of int option
```
```ocaml
type 'a wrapped_channel = 
  | Stateful_channel of comet_service * 'a chan_id
  | Stateless_channel of comet_service * 'a chan_id * stateless_kind
```
```ocaml
type 'a bus_send_service = 
  | Bus_send_service : (unit,
                       'a list,
                       Eliom_service.post,
                       Eliom_service.non_att,
                       Eliom_service.co,
                       Eliom_service.non_ext,
                       Eliom_service.reg,
                       [ `WithoutSuffix ],
                       unit,
                       [ `One of 'a list Eliom_parameter.ocaml ]
                         Eliom_parameter.param_name,
                       Eliom_service.non_ocaml)
                       Eliom_service.t -> 'a bus_send_service
```
```ocaml
type ('a, 'b) wrapped_bus = 'b wrapped_channel * 'a bus_send_service
```
