module ExprOrPatt :
  sig
    type tvarval = EPVstr of string | EPVvar of string
    type 'a tlist = PLEmpty | PLCons of 'a * 'a tlist
    type texprpatt =
        EPanyattr of tvarval * tvarval
      | EPanytag of string * texprpatt tlist * texprpatt tlist
      | EPpcdata of string
      | EPwhitespace of string
      | EPcomment of string
    (**/**)
    val loc : Lexing.position * Lexing.position
    val list_of_mlast_expr : MLast.expr list -> MLast.expr
    val list_of_mlast_patt : MLast.patt list -> MLast.patt
    val expr_valorval : tvarval -> MLast.expr
    val patt_valorval : tvarval -> MLast.patt
    val to_expr : texprpatt -> MLast.expr
    val to_expr_taglist : texprpatt tlist -> MLast.expr
    val to_expr_attlist : texprpatt tlist -> MLast.expr
    val to_patt : texprpatt -> MLast.patt
    val to_patt_taglist : texprpatt tlist -> MLast.patt
    val to_patt_attlist : texprpatt tlist -> MLast.patt
  end

(**/**)
val exprpatt_xml : ExprOrPatt.texprpatt Grammar.Entry.e
val exprpatt_any_tag : ExprOrPatt.texprpatt Grammar.Entry.e
val exprpatt_any_tag_list :
  ExprOrPatt.texprpatt ExprOrPatt.tlist Grammar.Entry.e
val exprpatt_any_attribute_list :
  ExprOrPatt.texprpatt ExprOrPatt.tlist Grammar.Entry.e
val exprpatt_attr_or_var : ExprOrPatt.tvarval Grammar.Entry.e
val exprpatt_value_or_var : ExprOrPatt.tvarval Grammar.Entry.e
val xml_exp : string -> MLast.expr
val xml_pat : string -> MLast.patt
val xmlparser : string -> ExprOrPatt.texprpatt ExprOrPatt.tlist
