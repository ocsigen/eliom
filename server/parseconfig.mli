val parse_size : string -> int64 option
val parse_string :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  string

(**/**)
val parser_config :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist list
val parse_server :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  unit
val extract_info :
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist ->
  (string * string) *
  ((string option * string option) option * int list * int list)
val parse_config :
  unit ->
  Simplexmlparser.ExprOrPatt.texprpatt Simplexmlparser.ExprOrPatt.tlist list
