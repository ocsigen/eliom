type mime_type = string
type extension = string

type mime_list

val parse_mime_types: string -> mime_list

val default_mime_list: unit -> mime_list
val default_mime_type : unit -> mime_type

val find_mime_type:
  ?default:mime_type -> mime_list:mime_list -> extension:extension -> mime_type

val find_mime_type_file:
  ?default:mime_type -> mime_list:mime_list -> filename:string -> mime_type
