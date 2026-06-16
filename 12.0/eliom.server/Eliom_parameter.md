
# Module `Eliom_parameter`

```ocaml
type suff = [ 
  | `WithoutSuffix
  | `WithSuffix
  | `Endsuffix
 ]
```
This type is used as a phantom type in [`params_type`](./#type-params_type) to describe whether a parameter is encoded in the path of the URI as a suffix parameter.

```ocaml
type ('a, +'b, 'c) params_type constraint 'b = [< suff ]
```
Abstract type for service parameters. See for example the parameter `~get_param` of `Eliom_service.Http.service`.

- ` 'a` is the type for the OCaml type of the parameter as expected by the service handler.
- ` 'b` is a phantom type, subtype of [`suff`](./#type-suff), stating the kind of the parameter: suffix or not.
- ` 'c` is the type of the parameter name, usually an instance of [`Eliom_parameter.param_name`](./#type-param_name), as used by forms construction functions (e.g., the last parameter of `Eliom_content.Html.D.get_form`), and specialized form widget (see for example the section `Form widget` of `Eliom_content.HTML5.D`). )

### Typed parameter's name

```ocaml
type +'a param_name
```
Abstract type for parameters' name. The `'a` type parameter is a phantom type, usually a subtype of [`setoneradio`](./#type-setoneradio), used to denotes the parameter's arity.

```ocaml
type no_param_name
```
Empty type used to denotes it is not possible to use the parameter in a form. See for example [`raw_post_data`](./#val-raw_post_data).

```ocaml
type +'a setoneradio = [ 
  | `Set of 'a
  | `One of 'a
  | `Radio of 'a
 ]
```
A parameter arity could either be:

- ``Set of 'a` means: any number of `'a`.
- ``One of 'a` means: exactly one `'a`.
- ``Radio of 'a` means: zero or one `'a`.
```ocaml
type +'a oneradio = [ 
  | `One of 'a
  | `Radio of 'a
 ]
```
Restriction of [`setoneradio`](./#type-setoneradio) unary and optional parameters.

```ocaml
type +'a setone = [ 
  | `Set of 'a
  | `One of 'a
 ]
```
Restriction of [`setoneradio`](./#type-setoneradio) unary and set parameters.


### Type helpers

```ocaml
type ('a, 'b) binsum = 
  | Inj1 of 'a
  | Inj2 of 'b
```
Helpers type used for parameters of type binary sum, see [`sum`](./#val-sum).

```ocaml
type 'an listnames = {
  it : 'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a;
}
```
Helpers type used to construct forms from lists, see [`list`](./#val-list).

```ocaml
type 'a to_and_of = {
  of_string : string -> 'a;
  to_string : 'a -> string;
}
```

### Basic types of pages parameters

```ocaml
val int : 
  string ->
  (int, [ `WithoutSuffix ], [ `One of int ] param_name) params_type
```
`int s` means that the service takes an integer as the parameter named `s`.

```ocaml
val int32 : 
  string ->
  (int32, [ `WithoutSuffix ], [ `One of int32 ] param_name) params_type
```
`int32 s` means that the service takes a 32-bit integer as the parameter named `s`.

```ocaml
val int64 : 
  string ->
  (int64, [ `WithoutSuffix ], [ `One of int64 ] param_name) params_type
```
`int64 s` means that the service takes a 64-bit integer as the parameter named `s`.

```ocaml
val float : 
  string ->
  (float, [ `WithoutSuffix ], [ `One of float ] param_name) params_type
```
`float s` means that the service takes a float as the parameter named `s`.

```ocaml
val string : 
  string ->
  (string, [ `WithoutSuffix ], [ `One of string ] param_name) params_type
```
`string s` means that the service takes a string as the parameter named `s`.

```ocaml
val bool : 
  string ->
  (bool, [ `WithoutSuffix ], [ `One of bool ] param_name) params_type
```
`bool s` means that the service takes a Boolean as the parameter named `s`. (To be used, for example, with Boolean checkboxes.)

```ocaml
val file : 
  string ->
  (Eliom_lib.file_info,
    [ `WithoutSuffix ],
    [ `One of Eliom_lib.file_info ] param_name)
    params_type
```
`file s` means that the service takes a file as the parameter named `s`.

```ocaml
val unit : (unit, [ `WithoutSuffix ], unit) params_type
```
Specifying parameter as `unit` is used for services that don't have any parameters

```ocaml
type coordinates = {
  abscissa : int;
  ordinate : int;
}
```
The type `coordinates` represents the data sent by an `<input type="image" ...>`.

```ocaml
val coordinates : 
  string ->
  (coordinates, [ `WithoutSuffix ], [ `One of coordinates ] param_name)
    params_type
```
`coordinates s` means that the service takes as parameters the coordinates of a point in an `<input type="image" ...>`.


### Composing types of pages parameters

```ocaml
val (**) : 
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [< `WithoutSuffix | `Endsuffix ] as 'e, 'd) params_type ->
  ('a * 'c, 'e, 'b * 'd) params_type
```
The combinator `p1 ** p2` allows one to define a service that takes a pair of parameters. The associated service handler should expect a pair `(p1, p2)`.

```ocaml
val prod : 
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [< `WithoutSuffix | `Endsuffix ] as 'e, 'd) params_type ->
  ('a * 'c, 'e, 'b * 'd) params_type
```
Same as [`(**)`](./#val-\(**\)).

```ocaml
val sum : 
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('c, [ `WithoutSuffix ], 'd) params_type ->
  (('a, 'c) binsum, [ `WithoutSuffix ], 'b * 'd) params_type
```
The combinator `sum p1 p2` allows one to define service that expect either the parameter `p1` or the parameter `p2`.

```ocaml
val opt : 
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a option, [ `WithoutSuffix ], 'b) params_type
```
The combinator `opt p` allows defining optional parameters.

```ocaml
val neopt : 
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a option, [ `WithoutSuffix ], 'b) params_type
```
The combinator `neopt p` allows defining an optional parameter assumed to be None if empty.

```ocaml
val radio : 
  (string -> ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
  string ->
  ('a option, [ `WithoutSuffix ], [ `Radio of 'b ] param_name) params_type
```
A parameter as `radio f s` specifies that the service takes an optional argument labeled `s`, of type `f s`. Use `radio` instead of [`opt`](./#val-opt) if you want to use this parameter with a radio button.

```ocaml
val any : ((string * string) list, [ `WithoutSuffix ], unit) params_type
```
Use this if you want to take any parameters. The service will answer to all the request, and get all parameters as an association list of strings.

```ocaml
val set : 
  (string -> ('a, [ `WithoutSuffix ], [ `One of 'b ] param_name) params_type) ->
  string ->
  ('a list, [ `WithoutSuffix ], [ `Set of 'b ] param_name) params_type
```
Use this if you want your service to take several parameters with the same name. The service handler will receive a list of values. To create the form, just use the same name several times. For example `set int "i"` will match the parameter string `i=4&i=22&i=111` and send to the service handler a list containing the three integers 4, 22 and 111\. The order is unspecified.

```ocaml
val list : 
  string ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a list, [ `WithoutSuffix ], 'b listnames) params_type
```
The service takes a list of parameters. The first parameter of this function is the name of the list. The service handler will receive a list of values. To create the form, an iterator of type [`Eliom_parameter.listnames`](./#type-listnames) is given to generate the name for each value.

```ocaml
val suffix : 
  ?redirect_if_not_suffix:bool ->
  ('s, [< `WithoutSuffix | `Endsuffix ], 'sn) params_type ->
  ('s, [ `WithSuffix ], 'sn) params_type
```
Tells that the parameter of the service handler is the suffix of the URL of the current service. e.g. `suffix (int "i" ** string "s")` will match an URL ending by `380/yo`. and send `(380, "yo")` to the service handler.

For each service with suffix, there is also a service with regular parameters (without suffix) that will be used if you create a form towards a service with suffix. If `redirect_if_not_suffix` is `true` (default), this service without suffix will be redirected to the suffix version.

```ocaml
val all_suffix : 
  string ->
  (string list, [ `Endsuffix ], [ `One of string list ] param_name) params_type
```
Takes the whole suffix, as long as possible, as a (slash separated) string list

```ocaml
val all_suffix_string : 
  string ->
  (string, [ `Endsuffix ], [ `One of string ] param_name) params_type
```
Takes the whole suffix, as long as possible, as a string

```ocaml
val suffix_prod : 
  ?redirect_if_not_suffix:bool ->
  ('s, [< `WithoutSuffix | `Endsuffix ], 'sn) params_type ->
  ('a, [ `WithoutSuffix ], 'an) params_type ->
  ('s * 'a, [ `WithSuffix ], 'sn * 'an) params_type
```
Tells that the function that will generate the service takes a pair whose first element is the suffix of the URL of the current service, and the second element corresponds to other (regular) parameters. e.g.: `suffix_prod (int "suff" ** all_suffix "endsuff") (int "i")` will match an URL ending by `777/go/go/go?i=320` and send the value `((777, ["go";"go";"go"]), 320)` to the service handler.

```ocaml
val suffix_const : 
  string ->
  (unit, [ `WithoutSuffix ], [ `One of unit ] param_name) params_type
```
`suffix_const v` is used only inside suffixes. It does nothing for regular parameters. It specifies that the service takes a constant parameter inside the suffix, whose value must be `v`. It is used for putting constant directory names inside suffix parameters (and thus allows suffix parameters that are anywhere in the path, e.g. `/param1/const/param2`).

```ocaml
type 'a ocaml
```
marshaled OCaml values of type 'a

```ocaml
val ocaml : 
  string ->
  'a Deriving_Json.t ->
  ('a, [ `WithoutSuffix ], [ `One of 'a ocaml ] param_name) params_type
```
`ocaml s` tells that the service is expecting some caml (client side) program to send some value of type 'a, marshaled. As usual `s` is the name of the parameter.

```ocaml
type raw_post_data =
  ((string * string) * (string * string) list) option * Cohttp_lwt.Body.t
```
When the content type is neither URLencoded form data nor multipart data, it is possible to get it as a stream of strings. The first element of the pair is the content-type. This kind of parameter cannot be combined with others. It is not possible to create a form towards a service taking such a parameter.

```ocaml
val raw_post_data : 
  (raw_post_data, [ `WithoutSuffix ], no_param_name) params_type
```

### Non localized parameters

```ocaml
type ('a, +'b, 'names) non_localized_params constraint 'b = [< suff ]
```
```ocaml
val make_non_localized_parameters : 
  prefix:string ->
  name:string ->
  ?persistent:bool ->
  ('a, [ `WithoutSuffix ], 'b) params_type ->
  ('a, [ `WithoutSuffix ], 'b) non_localized_params
```
Create a new specification for non localized parameters. You must give a name to this set of parameters. Warning: the names must be unique for the whole application. That's why the name is composed by a prefix (the name of your project) and another string (the name of your non localized parameters).

Will fail with exception `Failure _` if the name contains a dot. If `?persistent` is `true`, the non localized parameter may remain if you call another service, if this service allows this (default `false`).

```ocaml
type nl_params_set
```
Use this type to give non localized parameters to a link or a form

```ocaml
val empty_nl_params_set : nl_params_set
```
```ocaml
val add_nl_parameter : 
  nl_params_set ->
  ('a, [< `WithSuffix | `WithoutSuffix ], _) non_localized_params ->
  'a ->
  nl_params_set
```
```ocaml
val get_nl_params_names : 
  (_, [< `WithSuffix | `WithoutSuffix ], 'a) non_localized_params ->
  'a
```
```ocaml
val get_to_and_of : ('a, 'b, 'c) params_type -> 'a to_and_of
```
Given a parameter type, get the two functions that converts from and to strings. You should only use this function on

- options ;
- basic types : int, int32, int64, float, string
- marshal
- unit
- string
- bool
```ocaml
val user_type : 
  ?client_to_and_of:'a to_and_of Eliom_client_value.t ->
  of_string:(string -> 'a) ->
  to_string:('a -> string) ->
  string ->
  ('a, [ `WithoutSuffix ], [ `One of 'a ] param_name) params_type
```
`user_type ~of_string ~to_string s` construct a parameter, labeled `s`, such that the server will have to use `of_string` and `to_string` to make the conversion between the OCaml representation of the parameter and it's representation as a string. It allows one to use any type for a parameter. Providing converters via the optional `?client_to_and_from` parameter allows injecting the parameter (or a service that uses it) for use in client code.

```ocaml
val all_suffix_user : 
  ?client_to_and_of:'a to_and_of Eliom_client_value.t ->
  of_string:(string -> 'a) ->
  to_string:('a -> string) ->
  string ->
  ('a, [ `Endsuffix ], [ `One of 'a ] param_name) params_type
```
Takes the whole suffix, as long as possible, with a type specified by the user. See `user_type` for the description of the arguments.

```ocaml
val type_checker : 
  ('a -> unit) ->
  ('a, [< suff ] as 'b, 'c) params_type ->
  ('a, 'b, 'c) params_type
```
Specifying parameter as `type_checker check t` is equivalent as `t` but the check function is called after decoding the parameters, allowing you to make more checks on the parameters before the service handler is called. Raise an exception if the parameter is not correct, and the error handler will be called instead of the service handler.

```ocaml
val regexp : 
  Re.Pcre.regexp ->
  string ->
  to_string:(string -> string) ->
  string ->
  (string, [ `WithoutSuffix ], [ `One of string ] param_name) params_type
```
`regexp r d s` tells that the service takes a string that matches the regular expression `r` as parameter, labeled `s`, and that will be rewritten in d. The syntax of regexp is PCRE's one (uses then `Pcre` bindings). For example: `regexp (Re.Pcre.regexp "\[(.* )\]") "($1)" "myparam"` will match the parameter `myparam=[hello]` and send the string `"(hello)"` to the service handler.

```ocaml
val all_suffix_regexp : 
  Re.Pcre.regexp ->
  string ->
  to_string:(string -> string) ->
  string ->
  (string, [ `Endsuffix ], [ `One of string ] param_name) params_type
```
`all_suffix_regexp r d s` takes all the suffix, as long as possible, matching the regular expression `r`, name `s`, and rewrite it in `d`.

```ocaml
val get_non_localized_get_parameters : 
  ('a, [ `WithoutSuffix ], 'b) non_localized_params ->
  'a option
```
`get_non_localized_get_parameters ~sp p` decodes and returns non localized GET parameters specified by `p` if present.

```ocaml
val get_non_localized_post_parameters : 
  ('a, [ `WithoutSuffix ], 'b) non_localized_params ->
  'a option
```
`get_non_localized_post_parameters ~sp p` decodes and returns non localized POST parameters specified by `p` if present.
