type attr = [=`Attr of string | `CamlAttr of string ];
type valeur = [=`Val of string | `CamlVal of string ];
type attribute = [=`Attribute of (attr * valeur) | `CamlList of string ];
type token = 	
  [ Tag of (string * list attribute * bool)
	| PCData of string
	| Endtag of string
	| Comment of string
	| CamlString of string
        | CamlList of string
        | CamlExpr of string
	| Eof ];
