(*pp ${DERIVING} *)

type all deriving (Show,Json)

val print_all: all -> string
val extract_string: all -> string

val all: all
