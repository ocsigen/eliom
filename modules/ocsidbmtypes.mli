exception Ocsidbm_error

type query =
  | Get of string * string
  | Remove of string * string
  | Replace of string * string * string
  | Nextkey of string
  | Firstkey of string
        
type answer =
  | Ok
  | Dbm_not_found
  | Value of string
  | End
  | Key of string

