
# Module `Eliom_lib.String`

Improvement of module String


## Strings

```ocaml
type t = string
```
The type for strings.

```ocaml
val make : int -> char -> string
```
`make n c` is a string of length `n` with each index holding the character `c`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if n \< 0 or n \> Sys.max\_string\_length.
```ocaml
val init : int -> (int -> char) -> string
```
`init n f` is a string of length `n` with index `i` holding the character `f i` (called in increasing index order).

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if n \< 0 or n \> Sys.max\_string\_length.
since 4\.02
```ocaml
val empty : string
```
The empty string.

since 4\.13
```ocaml
val length : string -> int
```
`length s` is the length (number of bytes/characters) of `s`.

```ocaml
val get : string -> int -> char
```
`get s i` is the character at index `i` in `s`. This is the same as writing `s.[i]`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if i not an index of s.
```ocaml
val of_bytes : bytes -> string
```
Return a new string that contains the same bytes as the given byte sequence.

since 4\.13
```ocaml
val to_bytes : string -> bytes
```
Return a new byte sequence that contains the same bytes as the given string.

since 4\.13
```ocaml
val blit : string -> int -> bytes -> int -> int -> unit
```
Same as [`Bytes.blit_string`](./../../ocaml-compiler/stdlib/Stdlib-Bytes.md#val-blit_string) which should be preferred.


## Concatenating

**Note.** The [`Stdlib.(^)`](./../../ocaml-compiler/stdlib/Stdlib.md#val-\(^\)) binary operator concatenates two strings.

```ocaml
val concat : string -> string list -> string
```
`concat sep ss` concatenates the list of strings `ss`, inserting the separator string `sep` between each.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the result is longer than Sys.max\_string\_length bytes.
```ocaml
val cat : string -> string -> string
```
`cat s1 s2` concatenates s1 and s2 (`s1 ^ s2`).

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the result is longer than Sys.max\_string\_length bytes.
since 4\.13

## Predicates and comparisons

```ocaml
val equal : t -> t -> bool
```
`equal s0 s1` is `true` if and only if `s0` and `s1` are character-wise equal.

since 4\.03 (4.05 in StringLabels)
```ocaml
val compare : t -> t -> int
```
`compare s0 s1` sorts `s0` and `s1` in lexicographical order. `compare` behaves like [`Stdlib.compare`](./../../ocaml-compiler/stdlib/Stdlib.md#val-compare) on strings but may be more efficient.

```ocaml
val starts_with : prefix:string -> string -> bool
```
`starts_with ``~prefix s` is `true` if and only if `s` starts with `prefix`.

since 4\.13
```ocaml
val ends_with : suffix:string -> string -> bool
```
`ends_with ``~suffix s` is `true` if and only if `s` ends with `suffix`.

since 4\.13
```ocaml
val contains_from : string -> int -> char -> bool
```
`contains_from s start c` is `true` if and only if `c` appears in `s` after position `start`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if start is not a valid position in s.
```ocaml
val rcontains_from : string -> int -> char -> bool
```
`rcontains_from s stop c` is `true` if and only if `c` appears in `s` before position `stop+1`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if stop \< 0 or stop+1 is not a valid position in s.
```ocaml
val contains : string -> char -> bool
```
`contains s c` is [`String.contains_from`](./#val-contains_from)` s 0 c`.


## Extracting substrings

```ocaml
val sub : string -> int -> int -> string
```
`sub s pos len` is a string of length `len`, containing the substring of `s` that starts at position `pos` and has length `len`.

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if pos and len do not designate a valid substring of s.
```ocaml
val split_on_char : char -> string -> string list
```
`split_on_char sep s` is the list of all (possibly empty) substrings of `s` that are delimited by the character `sep`. If `s` is empty, the result is the singleton list `[""]`.

The function's result is specified by the following invariants:

- The list is not empty.
- Concatenating its elements using `sep` as a separator returns a string equal to the input (`concat (make 1 sep) (split_on_char sep s) = s`).
- No string in the result contains the `sep` character.
since 4\.04 (4.05 in StringLabels)

## Transforming

```ocaml
val map : (char -> char) -> string -> string
```
`map f s` is the string resulting from applying `f` to all the characters of `s` in increasing order.

since 4\.00
```ocaml
val mapi : (int -> char -> char) -> string -> string
```
`mapi f s` is like [`map`](./#val-map) but the index of the character is also passed to `f`.

since 4\.02
```ocaml
val fold_left : ('acc -> char -> 'acc) -> 'acc -> string -> 'acc
```
`fold_left f x s` computes `f (... (f (f x s.[0]) s.[1]) ...) s.[n-1]`, where `n` is the length of the string `s`.

since 4\.13
```ocaml
val fold_right : (char -> 'acc -> 'acc) -> string -> 'acc -> 'acc
```
`fold_right f s x` computes `f s.[0] (f s.[1] ( ... (f s.[n-1] x) ...))`, where `n` is the length of the string `s`.

since 4\.13
```ocaml
val for_all : (char -> bool) -> string -> bool
```
`for_all p s` checks if all characters in `s` satisfy the predicate `p`.

since 4\.13
```ocaml
val exists : (char -> bool) -> string -> bool
```
`exists p s` checks if at least one character of `s` satisfies the predicate `p`.

since 4\.13
```ocaml
val trim : string -> string
```
`trim s` is `s` without leading and trailing whitespace. Whitespace characters are: `' '`, `'\x0C'` (form feed), `'\n'`, `'\r'`, and `'\t'`.

since 4\.00
```ocaml
val escaped : string -> string
```
`escaped s` is `s` with special characters represented by escape sequences, following the lexical conventions of OCaml.

All characters outside the US-ASCII printable range \[0x20;0x7E\] are escaped, as well as backslash (0x2F) and double-quote (0x22).

The function [`Scanf.unescaped`](./../../ocaml-compiler/stdlib/Stdlib-Scanf.md#val-unescaped) is a left inverse of `escaped`, i.e. `Scanf.unescaped (escaped s) = s` for any string `s` (unless `escaped s` fails).

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if the result is longer than Sys.max\_string\_length bytes.
```ocaml
val uppercase_ascii : string -> string
```
`uppercase_ascii s` is `s` with all lowercase letters translated to uppercase, using the US-ASCII character set.

since 4\.03 (4.05 in StringLabels)
```ocaml
val lowercase_ascii : string -> string
```
`lowercase_ascii s` is `s` with all uppercase letters translated to lowercase, using the US-ASCII character set.

since 4\.03 (4.05 in StringLabels)
```ocaml
val capitalize_ascii : string -> string
```
`capitalize_ascii s` is `s` with the first character set to uppercase, using the US-ASCII character set.

since 4\.03 (4.05 in StringLabels)
```ocaml
val uncapitalize_ascii : string -> string
```
`uncapitalize_ascii s` is `s` with the first character set to lowercase, using the US-ASCII character set.

since 4\.03 (4.05 in StringLabels)

## Traversing

```ocaml
val iter : (char -> unit) -> string -> unit
```
`iter f s` applies function `f` in turn to all the characters of `s`. It is equivalent to `f s.[0]; f s.[1]; ...; f s.[length s - 1]; ()`.

```ocaml
val iteri : (int -> char -> unit) -> string -> unit
```
`iteri` is like [`iter`](./#val-iter), but the function is also given the corresponding character index.

since 4\.00

## Searching

```ocaml
val index_from : string -> int -> char -> int
```
`index_from s i c` is the index of the first occurrence of `c` in `s` after position `i`.

raises [`Not_found`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Not_found) if c does not occur in s after position i.
raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if i is not a valid position in s.
```ocaml
val index_from_opt : string -> int -> char -> int option
```
`index_from_opt s i c` is the index of the first occurrence of `c` in `s` after position `i` (if any).

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if i is not a valid position in s.
since 4\.05
```ocaml
val rindex_from : string -> int -> char -> int
```
`rindex_from s i c` is the index of the last occurrence of `c` in `s` before position `i+1`.

raises [`Not_found`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Not_found) if c does not occur in s before position i+1.
raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if i+1 is not a valid position in s.
```ocaml
val rindex_from_opt : string -> int -> char -> int option
```
`rindex_from_opt s i c` is the index of the last occurrence of `c` in `s` before position `i+1` (if any).

raises [`Invalid_argument`](./../../ocaml-compiler/stdlib/Stdlib.md#exception-Invalid_argument) if i+1 is not a valid position in s.
since 4\.05
```ocaml
val index : string -> char -> int
```
`index s c` is [`String.index_from`](./#val-index_from)` s 0 c`.

```ocaml
val index_opt : string -> char -> int option
```
`index_opt s c` is [`String.index_from_opt`](./#val-index_from_opt)` s 0 c`.

since 4\.05
```ocaml
val rindex : string -> char -> int
```
`rindex s c` is [`String.rindex_from`](./#val-rindex_from)` s (length s - 1) c`.

```ocaml
val rindex_opt : string -> char -> int option
```
`rindex_opt s c` is [`String.rindex_from_opt`](./#val-rindex_from_opt)` s (length s - 1) c`.

since 4\.05

## Strings and Sequences

```ocaml
val to_seq : t -> char Seq.t
```
`to_seq s` is a sequence made of the string's characters in increasing order.

since 4\.07
```ocaml
val to_seqi : t -> (int * char) Seq.t
```
`to_seqi s` is like [`to_seq`](./#val-to_seq) but also tuples the corresponding index.

since 4\.07
```ocaml
val of_seq : char Seq.t -> t
```
`of_seq s` is a string made of the sequence's characters.

since 4\.07

## UTF decoding and validations

since 4\.14

### UTF-8

```ocaml
val get_utf_8_uchar : t -> int -> Uchar.utf_decode
```
`get_utf_8_uchar b i` decodes an UTF-8 character at index `i` in `b`.

```ocaml
val is_valid_utf_8 : t -> bool
```
`is_valid_utf_8 b` is `true` if and only if `b` contains valid UTF-8 data.


### UTF-16BE

```ocaml
val get_utf_16be_uchar : t -> int -> Uchar.utf_decode
```
`get_utf_16be_uchar b i` decodes an UTF-16BE character at index `i` in `b`.

```ocaml
val is_valid_utf_16be : t -> bool
```
`is_valid_utf_16be b` is `true` if and only if `b` contains valid UTF-16BE data.


### UTF-16LE

```ocaml
val get_utf_16le_uchar : t -> int -> Uchar.utf_decode
```
`get_utf_16le_uchar b i` decodes an UTF-16LE character at index `i` in `b`.

```ocaml
val is_valid_utf_16le : t -> bool
```
`is_valid_utf_16le b` is `true` if and only if `b` contains valid UTF-16LE data.


## Spellchecking

```ocaml
val edit_distance : ?limit:int -> t -> t -> int
```
`edit_distance s0 s1` is the number of single character edits (understood as insertion, deletion, substitution, transposition) that are needed to change `s0` into `s1`.

If `limit` is provided the function returns with `limit` as soon as it was determined that `s0` and `s1` have distance of at least `limit`. This is faster if you have a fixed limit, for example for spellchecking.

The function assumes the strings are UTF-8 encoded and uses [`Uchar.t`](./../../ocaml-compiler/stdlib/Stdlib-Uchar.md#type-t) for the notion of character. Decoding errors are replaced by [`Uchar.rep`](./../../ocaml-compiler/stdlib/Stdlib-Uchar.md#val-rep). Normalizing the strings to [NFC](https://unicode.org/glossary/#normalization_form_c) gives better results.

**Note.** This implements the simpler Optimal String Alignement (OSA) distance, not the Damerau-Levenshtein distance. With this function `"ca"` and `"abc"` have a distance of 3 not 2\.

since 5\.4
```ocaml
val spellcheck : 
  ?max_dist:(string -> int) ->
  ((string -> unit) -> unit) ->
  string ->
  string list
```
`spellcheck iter_dict s` are the strings enumerated by the iterator `iter_dict` whose [edit distance](./#val-edit_distance) to `s` is the smallest and at most `max_dist s`. If multiple corrections are returned their order is as found in `iter_dict`. The default `max_dist s` is:

- `0` if `s` has 0 to 2 Unicode characters.
- `1` if `s` has 3 to 4 Unicode characters.
- `2` otherwise.
If your dictionary is a list `l`, a suitable `iter_dict` is given by `(fun yield -> List.iter yield l)`.

All strings are assumed to be UTF-8 encoded, decoding errors are replaced by [`Uchar.rep`](./../../ocaml-compiler/stdlib/Stdlib-Uchar.md#val-rep) characters.

since 5\.4

## Binary decoding of integers

The functions in this section binary decode integers from strings.

All following functions raise `Invalid_argument` if the characters needed at index `i` to decode the integer are not available.

Little-endian (resp. big-endian) encoding means that least (resp. most) significant bytes are stored first. Big-endian is also known as network byte order. Native-endian encoding is either little-endian or big-endian depending on [`Sys.big_endian`](./../../ocaml-compiler/stdlib/Stdlib-Sys.md#val-big_endian).

32-bit and 64-bit integers are represented by the `int32` and `int64` types, which can be interpreted either as signed or unsigned numbers.

8-bit and 16-bit integers are represented by the `int` type, which has more bits than the binary encoding. These extra bits are sign-extended (or zero-extended) for functions which decode 8-bit or 16-bit integers and represented them with `int` values.

```ocaml
val get_uint8 : string -> int -> int
```
`get_uint8 b i` is `b`'s unsigned 8-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int8 : string -> int -> int
```
`get_int8 b i` is `b`'s signed 8-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_uint16_ne : string -> int -> int
```
`get_uint16_ne b i` is `b`'s native-endian unsigned 16-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_uint16_be : string -> int -> int
```
`get_uint16_be b i` is `b`'s big-endian unsigned 16-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_uint16_le : string -> int -> int
```
`get_uint16_le b i` is `b`'s little-endian unsigned 16-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int16_ne : string -> int -> int
```
`get_int16_ne b i` is `b`'s native-endian signed 16-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int16_be : string -> int -> int
```
`get_int16_be b i` is `b`'s big-endian signed 16-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int16_le : string -> int -> int
```
`get_int16_le b i` is `b`'s little-endian signed 16-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int32_ne : string -> int -> int32
```
`get_int32_ne b i` is `b`'s native-endian 32-bit integer starting at character index `i`.

since 4\.13
```ocaml
val hash : t -> int
```
An unseeded hash function for strings, with the same output value as [`Hashtbl.hash`](./../../ocaml-compiler/stdlib/Stdlib-Hashtbl.md#val-hash). This function allows this module to be passed as argument to the functor [`Hashtbl.Make`](./../../ocaml-compiler/stdlib/Stdlib-Hashtbl-Make.md).

since 5\.0
```ocaml
val seeded_hash : int -> t -> int
```
A seeded hash function for strings, with the same output value as [`Hashtbl.seeded_hash`](./../../ocaml-compiler/stdlib/Stdlib-Hashtbl.md#val-seeded_hash). This function allows this module to be passed as argument to the functor [`Hashtbl.MakeSeeded`](./../../ocaml-compiler/stdlib/Stdlib-Hashtbl-MakeSeeded.md).

since 5\.0
```ocaml
val get_int32_be : string -> int -> int32
```
`get_int32_be b i` is `b`'s big-endian 32-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int32_le : string -> int -> int32
```
`get_int32_le b i` is `b`'s little-endian 32-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int64_ne : string -> int -> int64
```
`get_int64_ne b i` is `b`'s native-endian 64-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int64_be : string -> int -> int64
```
`get_int64_be b i` is `b`'s big-endian 64-bit integer starting at character index `i`.

since 4\.13
```ocaml
val get_int64_le : string -> int -> int64
```
`get_int64_le b i` is `b`'s little-endian 64-bit integer starting at character index `i`.

since 4\.13
```ocaml
val remove_spaces : string -> int -> int -> string
```
`remove_spaces s beg endd` returns a copy of the string from beg to endd, removing spaces at the beginning and at the end

```ocaml
val basic_sep : char -> string -> string * string
```
Cuts a string to the next separator

```ocaml
val sep : char -> string -> string * string
```
Cuts a string to the next separator, removing spaces. Raises `Not_found` if the separator cannot be found.

```ocaml
val split : ?multisep:bool -> char -> string -> string list
```
Splits a string for words with separator, removing spaces. For ex "azert, sdfmlskdf, dfdsfs".

```ocaml
val may_append : string -> sep:string -> string -> string
```
```ocaml
val may_concat : string -> sep:string -> string -> string
```
```ocaml
val first_diff : string -> string -> int -> int -> int
```
`first_diff s1 s2 n last` returns the index of the first difference between s1 and s2, starting from n and ending at last. returns (last \+ 1\) if no difference is found.

```ocaml
module Table : Map.S with type key = string
```
```ocaml
module Set : Set.S with type elt = string
```
```ocaml
module Map : Map.S with type key = string
```