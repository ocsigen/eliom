(* Ocsigen
 * Copyright (C) 2010 Jérome Vouillon
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

type t
type flag =
  | Global_search (* g *)
  | Case_insensitive (* i *)
  | Multi_line (* m *)

external make : string -> string -> t = "caml_regexp_make"
external last_index : t -> int = "caml_regexp_last_index"

let make
    ?(global = false)
    ?(case_insensitive = false)
    ?(multi_line = false)
    expr =
  make expr
    (""
     ^ (if global then "g" else "")
     ^ (if case_insensitive then "i" else "")
     ^ (if multi_line then "m" else ""))

external test : t -> string -> bool = "caml_regexp_test"

(** executes a match
    the result is an array of substrings corresponding to matched groups
    0 is the whole substring matched by the regexp
    1 is the outermost parenthetised group
    etc.
*)
external exec : t -> string -> string array = "caml_regexp_exec"

(** returns the index of the first match of the regexp in the string
    raises Not_found if the string is not matched by the regexp
*)
external index : t -> string -> int = "caml_regexp_index"

(** replace [regexp] [substitution] [string]
    special chars (doc from MDC):
    - $$
        Inserts a "$".
    - $&
        Inserts the matched substring.
    - $`
        Inserts the portion of the string that precedes the matched substring.
    - $'
        Inserts the portion of the string that follows the matched substring.
    - $n or $nn  Where n or nn are decimal digits
        Inserts the nth parenthesized submatch string, provided the first argument was a RegExp object.
*)
external replace : t -> string -> string -> string = "caml_regexp_replace"

(** replace_fun [regexp] [substitution function] [string]
    the substitution function takes :
      - the offset of the current match
      - an array of matched groups (0 = total curren match, see [exec])
    WARNING: uses callback mechanism which is not "au point"
*)
external replace_fun : t -> (int -> string array -> string) -> string -> string = "caml_regexp_replace_fun"

external split : t -> string -> string array = "caml_regexp_split"
