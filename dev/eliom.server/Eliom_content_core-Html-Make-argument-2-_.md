# Parameter `Make._`

```ocaml
module Xml = Xml
```
```ocaml
val string_of_big_variant : ([< Html_types.big_variant ], string) Xml.W.ft
```
```ocaml
val string_of_bool : (bool, string) Xml.W.ft
```
```ocaml
val onoff_of_bool : (bool, string) Xml.W.ft
```
```ocaml
val string_of_character : (Html_types.character, string) Xml.W.ft
```
```ocaml
val string_of_input_type : ([< Html_types.input_type ], string) Xml.W.ft
```
```ocaml
val string_of_script_type : ([< Html_types.script_type ], string) Xml.W.ft
```
```ocaml
val string_of_number_or_datetime : 
  ([< Html_types.number_or_datetime ], string) Xml.W.ft
```
```ocaml
val string_of_linktypes : ([< Html_types.linktype ] list, string) Xml.W.ft
```
```ocaml
val string_of_mediadesc : 
  ([< Html_types.mediadesc_token ] list, string) Xml.W.ft
```
```ocaml
val string_of_referrerpolicy : 
  ([< Html_types.referrerpolicy ], string) Xml.W.ft
```
```ocaml
val string_of_numbers : (Html_types.numbers, string) Xml.W.ft
```
```ocaml
val string_of_sandbox : ([< Html_types.sandbox_token ] list, string) Xml.W.ft
```
```ocaml
val string_of_sizes : 
  ((Html_types.number * Html_types.number) list option, string) Xml.W.ft
```
```ocaml
type image_candidate = [ 
  | `Url of Xml.uri
  | `Url_width of Xml.uri * Html_types.number
  | `Url_pixel of Xml.uri * Html_types.float_number
 ]
```
```ocaml
val string_of_srcset : ([< image_candidate ] list, string) Xml.W.ft
```
```ocaml
val string_of_autocomplete : (Html_types.autocomplete_option, string) Xml.W.ft
```
```ocaml
val string_of_step : (float option, string) Xml.W.ft
```
```ocaml
val unoption_string : (string option, string) Xml.W.ft
```
