type mime_type = string
type extension = string

type mime_assoc

val parse_mime_types: string -> mime_assoc

val default_mime_assoc: unit -> mime_assoc
val default_mime_type : unit -> mime_type

val find_mime_type:
  ?default:mime_type -> mime_assoc:mime_assoc -> extension:extension -> mime_type

val find_mime_type_file:
  ?default:mime_type -> mime_assoc:mime_assoc -> filename:string -> mime_type

val update_mime:
  mime_assoc:mime_assoc -> extension:string -> mime:mime_type -> mime_assoc
