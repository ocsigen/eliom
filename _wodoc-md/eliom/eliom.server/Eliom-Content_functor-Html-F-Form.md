
# Module `F.Form`

```ocaml
type 'a param
```
```ocaml
val float : float param
```
```ocaml
val int : int param
```
```ocaml
val int32 : int32 param
```
```ocaml
val int64 : int64 param
```
```ocaml
val nativeint : nativeint param
```
```ocaml
val bool : bool param
```
```ocaml
val string : string param
```
```ocaml
val user : ('a -> string) -> 'a param
```
```ocaml
val make_post_uri_components : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  service:
    ('get,
      'post,
      Service.post,
      'a,
      'b,
      'c,
      'd,
      [< `WithSuffix | `WithoutSuffix ],
      'e,
      'f,
      'g)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?keep_get_na_params:bool ->
  'get ->
  'post ->
  string
  * (string * Mod_parameters.param) list
  * string option
  * (string * Mod_parameters.param) list
```
```ocaml
val get_form : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:[< Html_types.form_attrib ] Arg.attrib list ->
  service:
    ('a,
      unit,
      Service.get,
      'b,
      'c,
      'd,
      'e,
      [< `WithSuffix | `WithoutSuffix ],
      'gn,
      'f,
      Service.non_ocaml)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?xhr:bool ->
  ('gn -> [< Html_types.form_content ] Arg.elt list) ->
  [> Html_types.form ] Arg.elt
```
```ocaml
val lwt_get_form : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:[< Html_types.form_attrib ] Arg.attrib list ->
  service:
    ('a,
      unit,
      Service.get,
      'b,
      'c,
      'd,
      'e,
      [< `WithSuffix | `WithoutSuffix ],
      'gn,
      'f,
      Service.non_ocaml)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?nl_params:Parameter.nl_params_set ->
  ?xhr:bool ->
  ('gn -> [< Html_types.form_content ] Arg.elt list Lwt.t) ->
  [> Html_types.form ] Arg.elt Lwt.t
```
```ocaml
val post_form : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:[< Html_types.form_attrib ] Arg.attrib list ->
  service:
    ('get,
      'a,
      Service.post,
      'b,
      'c,
      'd,
      'e,
      [< `WithSuffix | `WithoutSuffix ],
      'f,
      'pn,
      Service.non_ocaml)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?keep_get_na_params:bool ->
  ?nl_params:Parameter.nl_params_set ->
  ?xhr:bool ->
  ('pn -> [< Html_types.form_content ] Arg.elt list) ->
  'get ->
  [> Html_types.form ] Arg.elt
```
```ocaml
val lwt_post_form : 
  ?absolute:bool ->
  ?absolute_path:bool ->
  ?https:bool ->
  ?a:[< Html_types.form_attrib ] Arg.attrib list ->
  service:
    ('get,
      'a,
      Service.post,
      'b,
      'c,
      'd,
      'e,
      [< `WithSuffix | `WithoutSuffix ],
      'f,
      'pn,
      Service.non_ocaml)
      Service.t ->
  ?hostname:string ->
  ?port:int ->
  ?fragment:string ->
  ?keep_nl_params:[ `All | `None | `Persistent ] ->
  ?keep_get_na_params:bool ->
  ?nl_params:Parameter.nl_params_set ->
  ?xhr:bool ->
  ('pn -> [< Html_types.form_content ] Arg.elt list Lwt.t) ->
  'get ->
  [> Html_types.form ] Arg.elt Lwt.t
```
```ocaml
val input : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  input_type:[< Html_types.input_type ] ->
  ?name:[< 'a Parameter.setoneradio ] Parameter.param_name ->
  ?value:'a ->
  'a param ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val file_input : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  name:[< Lib.file_info Parameter.setoneradio ] Parameter.param_name ->
  unit ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val image_input : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  name:[< Parameter.coordinates Parameter.oneradio ] Parameter.param_name ->
  ?src:Arg.uri ->
  unit ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val checkbox : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  ?checked:bool ->
  name:[ `Set of 'a ] Parameter.param_name ->
  value:'a ->
  'a param ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val bool_checkbox_one : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  ?checked:bool ->
  name:[ `One of bool ] Parameter.param_name ->
  unit ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val radio : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  ?checked:bool ->
  name:[ `Radio of 'a ] Parameter.param_name ->
  value:'a ->
  'a param ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val string_radio_required : 
  ?a:[< Html_types.input_attrib ] Arg.attrib list ->
  ?checked:bool ->
  name:[ `One of string ] Parameter.param_name ->
  value:string ->
  unit ->
  [> Html_types.input ] Arg.elt
```
```ocaml
val button : 
  ?a:[< Html_types.button_attrib ] Arg.attrib list ->
  button_type:[< Eliom__Form_sigs.button_type ] ->
  name:[< 'a Parameter.setone ] Parameter.param_name ->
  value:'a ->
  'a param ->
  Html_types.button_content Arg.elt list ->
  [> Html_types.button ] Arg.elt
```
```ocaml
val button_no_value : 
  ?a:[< Html_types.button_attrib ] Arg.attrib list ->
  button_type:[< Eliom__Form_sigs.button_type ] ->
  Html_types.button_content Arg.elt list ->
  [> Html_types.button ] Arg.elt
```
```ocaml
val textarea : 
  ?a:[< Html_types.textarea_attrib ] Arg.attrib list ->
  name:[< string Parameter.setoneradio ] Parameter.param_name ->
  ?value:string ->
  unit ->
  [> Html_types.textarea ] Arg.elt
```
```ocaml
type !'a soption =
  Html_types.option_attrib Arg.attrib list
  * 'a
  * Html_types.pcdata Arg.elt option
  * bool
```
```ocaml
type !'a select_opt = 
  | Optgroup of [ `Accesskey
              | `Aria
              | `Class
              | `Contenteditable
              | `Contextmenu
              | `Dir
              | `Disabled
              | `Draggable
              | `Hidden
              | `Id
              | `Lang
              | `OnAbort
              | `OnBlur
              | `OnCanPlay
              | `OnCanPlayThrough
              | `OnChange
              | `OnClick
              | `OnClose
              | `OnContextMenu
              | `OnDblClick
              | `OnDrag
              | `OnDragEnd
              | `OnDragEnter
              | `OnDragLeave
              | `OnDragOver
              | `OnDragStart
              | `OnDrop
              | `OnDurationChange
              | `OnEmptied
              | `OnEnded
              | `OnError
              | `OnFocus
              | `OnFormChange
              | `OnFormInput
              | `OnInput
              | `OnInvalid
              | `OnKeyDown
              | `OnKeyPress
              | `OnKeyUp
              | `OnLoad
              | `OnLoadStart
              | `OnLoadedData
              | `OnLoadedMetaData
              | `OnMouseDown
              | `OnMouseMove
              | `OnMouseOut
              | `OnMouseOver
              | `OnMouseUp
              | `OnMouseWheel
              | `OnPause
              | `OnPlay
              | `OnPlaying
              | `OnProgress
              | `OnRateChange
              | `OnReadyStateChange
              | `OnScroll
              | `OnSeeked
              | `OnSeeking
              | `OnSelect
              | `OnShow
              | `OnStalled
              | `OnSubmit
              | `OnSuspend
              | `OnTimeUpdate
              | `OnTouchCancel
              | `OnTouchEnd
              | `OnTouchMove
              | `OnTouchStart
              | `OnVolumeChange
              | `OnWaiting
              | `Role
              | `Spellcheck
              | `Style_Attr
              | `Tabindex
              | `Title
              | `Translate
              | `User_data
              | `XML_lang
              | `XMLns ]
                Arg.attrib
                list
    * string
    * 'a soption
    * 'a soption list
  | Option of 'a soption
```
```ocaml
val select : 
  ?a:[< Html_types.select_attrib ] Arg.attrib list ->
  ?required:Html_types.pcdata Arg.elt ->
  name:[ `One of 'a ] Parameter.param_name ->
  'a param ->
  'a select_opt ->
  'a select_opt list ->
  [> Html_types.select ] Arg.elt
```
```ocaml
val multiple_select : 
  ?a:[< Html_types.select_attrib ] Arg.attrib list ->
  ?required:Html_types.pcdata Arg.elt ->
  name:[ `Set of 'a ] Parameter.param_name ->
  'a param ->
  'a select_opt ->
  'a select_opt list ->
  [> Html_types.select ] Arg.elt
```