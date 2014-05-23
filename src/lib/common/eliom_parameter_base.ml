(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
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

open Eliom_lib

(** Type of names in a form *)
type 'a param_name = string

(** empty type used when it is not possible to use the parameter in a form *)
type no_param_name

type ('a,'b) binsum = Inj1 of 'a | Inj2 of 'b;;

type 'an listnames =
    {it:'el 'a. ('an -> 'el -> 'a -> 'a) -> 'el list -> 'a -> 'a}

type coordinates =
    {abscissa: int;
     ordinate: int}

type 'a setoneradio = [ `Set of 'a | `One of 'a | `Radio of 'a ]
type 'a oneradio = [ `One of 'a | `Radio of 'a ]
type 'a setone = [ `Set of 'a | `One of 'a ]

(*****************************************************************************)
(* This is a generalized algebraic datatype *)
(* Use only with constructors from eliom.ml *)
    (* 'tipo is [`WithSuffix] or [`WithoutSuffix] *)
type ('a, 'tipo, +'names) params_type =
    TProd of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = 'a1 * 'a2 ; 'names = 'names1 * 'names2 *)
  | TOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TNEOption of (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 option *)
  | TList of string * (* 'a1 *) ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSet of ('a,'tipo,'names) params_type (* 'a = 'a1 list *)
  | TSum of (* 'a1 *) ('a,'tipo,'names) params_type * (* 'a2 *) ('a,'tipo,'names) params_type (* 'a = ('a1, 'a2) binsum *)
  | TString of string (* 'a = string *)
  | TInt of string (* 'a = int *)
  | TInt32 of string (* 'a = int32 *)
  | TInt64 of string (* 'a = int64 *)
  | TFloat of string (* 'a = float *)
  | TBool of string (* 'a = bool *)
  | TFile of string (* 'a = file_info *)
  | TUserType of string * (string -> 'a) * ('a -> string) (* 'a = 'a *)
  | TTypeFilter of ('a, 'tipo, 'names) params_type * ('a -> unit) option
      (* The filter does not change the values.
         It only checks they are ok.
         Would be better to do a filter that also translates the types.
         But we want something that can be sent to the client:
         difficult to send a function ...
      *)
  | TCoord of string (* 'a = 'a1 *)
  | TCoordv of ('a,'tipo,'names) params_type * string
  | TESuffix of string (* 'a = string list *)
  | TESuffixs of string (* 'a = string *)
  | TESuffixu of (string * (string -> 'a) * ('a -> string)) (* 'a = 'a *)
  | TSuffix of (bool * ('a,'tipo, 'names) params_type) (* 'a = 'a1 *)
      (* bool = redirect the version without suffix to the suffix version *)
  | TUnit (* 'a = unit *)
  | TAny (* 'a = (string * string) list *)
  | TConst of string (* 'a = unit; 'names = unit *)
  | TNLParams of ('a, 'tipo, 'names) non_localized_params
  | TJson of string * 'a Deriving_Json.t option (* 'a = '_b ocaml *)
(* It is an option but always Some ... server side,
   and always None client side.*)
  | TRaw_post_data

and ('a, 'tipo, +'names) non_localized_params =
    string *
      bool (* persistent *) *
      ('a option Polytables.key * 'a option Polytables.key) *
      ('a, 'tipo, 'names) params_type


(* As GADT are not implemented in OCaml for now, we define our own
   constructors for params_type *)
let int (n : string) : (int, [`WithoutSuffix], [ `One of int ] param_name) params_type =
  TInt n

let int32 (n : string) : (int32, [`WithoutSuffix], [ `One of int32 ] param_name) params_type =
  TInt32 n

let int64 (n : string) : (int64, [`WithoutSuffix], [ `One of int64 ] param_name) params_type =
  TInt64 n

let float (n : string)
    : (float, [`WithoutSuffix], [ `One of float ] param_name) params_type =
  TFloat n

let bool (n : string)
    : (bool, [`WithoutSuffix], [ `One of bool ] param_name) params_type
    = TBool n

let string (n : string)
    : (string, [`WithoutSuffix], [ `One of string ] param_name) params_type =
  TString n

let file (n : string)
    : (file_info , [`WithoutSuffix], [ `One of file_info ] param_name) params_type =
  TFile n

let unit : (unit, [`WithoutSuffix], unit) params_type = TUnit


let type_checker
    check
    (t : ('a, 's, 'an) params_type)
    : ('a, 's, 'an) params_type =
  Obj.magic (TTypeFilter (t, Some check))

let user_type
    ~(of_string : string -> 'a) ~(to_string : 'a -> string) (n : string)
    : ('a,[`WithoutSuffix], [ `One of 'a ] param_name) params_type =
  Obj.magic (TUserType (n, of_string, to_string))

let sum (t1 : ('a, [`WithoutSuffix], 'an) params_type)
    (t2 : ('b,[`WithoutSuffix], 'bn) params_type)
    : (('a,'b) binsum, [`WithoutSuffix], 'an * 'bn ) params_type =
  Obj.magic (TSum (Obj.magic t1, Obj.magic t2))

let prod (t1 : ('a, [`WithoutSuffix], 'an) params_type)
    (t2 : ('b, [<`WithoutSuffix|`Endsuffix] as 'e, 'bn) params_type)
    : (('a * 'b),'e, 'an * 'bn) params_type =
  Obj.magic (TProd ((Obj.magic t1), (Obj.magic t2)))

let ( ** ) = prod

let coordinates (n : string)
    : (coordinates, [`WithoutSuffix], [ `One of coordinates ] param_name) params_type =
  TCoord n

let string_coordinates (n : string)
    : (string * coordinates,
       [`WithoutSuffix],
       [ `One of (string * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (string n, n))

let int_coordinates (n : string)
    : (int * coordinates,
       [`WithoutSuffix],
       [ `One of (int * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (int n, n))

let int32_coordinates (n : string)
    : (int32 * coordinates,
       [`WithoutSuffix],
       [ `One of (int32 * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (int32 n, n))

let int64_coordinates (n : string)
    : (int64 * coordinates,
       [`WithoutSuffix],
       [ `One of (int64 * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (int64 n, n))

let float_coordinates (n : string)
    : (float * coordinates,
       [`WithoutSuffix],
       [ `One of (float * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (float n, n))

let user_type_coordinates
    ~(of_string : string -> 'a) ~(to_string : 'a -> string) (n : string)
    : ('a * coordinates,
       [`WithoutSuffix],
       [ `One of ('a * coordinates) ] param_name) params_type =
  Obj.magic (TCoordv (user_type of_string to_string n, n))

let opt (t : ('a, [`WithoutSuffix], 'an) params_type)
    : ('a option,[`WithoutSuffix], 'an) params_type =
  Obj.magic (TOption t)

let neopt (t : ('a, [`WithoutSuffix], 'an) params_type)
    : ('a option,[`WithoutSuffix], 'an) params_type =
  Obj.magic (TNEOption t)

let radio (t : string ->
            ('a, [`WithoutSuffix], [ `One of 'an ] param_name) params_type)
    (n : string)
    : ('a option, [`WithoutSuffix], [ `Radio of 'an ] param_name) params_type =
  Obj.magic (TOption (t n))

let list (n : string) (t : ('a, [`WithoutSuffix], 'an) params_type)
    : ('a list, [`WithoutSuffix], 'an listnames) params_type =
  Obj.magic (TList (n,t))

let set (t : string -> ('a, [`WithoutSuffix], [ `One of 'an ] param_name) params_type) (n : string)
    : ('a list, [`WithoutSuffix], [ `Set of 'an ] param_name) params_type =
  Obj.magic (TSet (t n))

let any
    : ((string * string) list, [`WithoutSuffix], unit) params_type =
  TAny

let suffix_const (v : string)
    : (unit, [`WithoutSuffix], [ `One of unit ] param_name) params_type =
  TConst v


let all_suffix (n : string) :
    (string list , [`Endsuffix],
     [ `One of string list ] param_name) params_type =
  (Obj.magic (TESuffix n))

let all_suffix_string (n : string) :
    (string, [`Endsuffix], [ `One of string ] param_name) params_type =
  (Obj.magic (TESuffixs n))

let all_suffix_user
    ~(of_string : string -> 'a) ~(to_string : 'a -> string) (n : string) :
    ('a, [`Endsuffix], [ `One of 'a ] param_name) params_type =
  (Obj.magic (TESuffixu (n, of_string, to_string)))

let suffix ?(redirect_if_not_suffix = true)
    (s : ('s, [<`WithoutSuffix|`Endsuffix], 'sn) params_type) :
    ('s , [`WithSuffix], 'sn) params_type =
  (Obj.magic (TSuffix (redirect_if_not_suffix, s)))

let suffix_prod ?(redirect_if_not_suffix = true)
    (s : ('s, [<`WithoutSuffix|`Endsuffix], 'sn) params_type)
    (t : ('a, [`WithoutSuffix], 'an) params_type) :
    (('s * 'a), [`WithSuffix], 'sn * 'an) params_type =
  (Obj.magic (TProd (Obj.magic (TSuffix (redirect_if_not_suffix, s)),
                                Obj.magic t)))


type 'a ocaml = string (* marshaled values of type 'a *)

let ocaml (n : string) typ
    : ('a, [`WithoutSuffix], [ `One of 'a ocaml ] param_name) params_type =
  TJson (n, Some typ)

type raw_post_data =
  ((string * string) * (string * string) list) option *
      string Ocsigen_stream.t option

let raw_post_data = TRaw_post_data

(******************************************************************)
let make_list_suffix i = "["^(string_of_int i)^"]"

(******************************************************************)
(* The following function takes a 'a params_type and a 'a and
   constructs the list of parameters (GET or POST)
   (This is a marshalling function towards HTTP parameters format) *)
let construct_params_list_raw
    nlp
    (typ : ('a, [<`WithSuffix|`WithoutSuffix],'b) params_type)
    (params : 'a) =
  let rec make_suffix typ params =
    match typ with
    | TNLParams (_, _, _, t) -> make_suffix t params
    | TProd (TList _, _)
    | TProd (TSet _, _) ->
        failwith "Lists or sets in suffixes must be last parameters"
    | TProd (t1, t2) ->
        (make_suffix t1 (fst (Obj.magic params)))@
        (make_suffix t2 (snd (Obj.magic params)))
    | TString _ -> [Obj.magic params]
    | TInt _ -> [string_of_int (Obj.magic params)]
    | TInt32 _ -> [Int32.to_string (Obj.magic params)]
    | TInt64 _ -> [Int64.to_string (Obj.magic params)]
    | TFloat _ -> [string_of_float (Obj.magic params)]
    | TBool _ -> [string_of_bool (Obj.magic params)]
    | TUnit -> [""]
    | TConst v -> [Obj.magic v]
    | TOption t -> (match Obj.magic params with
                      | None -> [""]
                      | Some v -> make_suffix t v)
    | TNEOption t -> (match Obj.magic params with
                      | None -> [""]
                      | Some v -> make_suffix t v)
    | TList (_, t) | TSet t ->
        (match params with
          | [] -> [""]
          | a::l ->
              (make_suffix t (Obj.magic a))@
                (make_suffix typ (Obj.magic l)))
    | TUserType (_, of_string, string_of) -> [string_of (Obj.magic params)]
    | TTypeFilter (t, check) -> (make_suffix t (Obj.magic params))
    | TSum (t1, t2) ->
        (match Obj.magic params with
           | Inj1 p -> make_suffix t1 p
           | Inj2 p -> make_suffix t2 p)
    | TCoord _ ->
        (make_suffix (TInt "") (Obj.magic (Obj.magic params).abscissa))@
        (make_suffix (TInt "") (Obj.magic (Obj.magic params).ordinate))
    | TCoordv (t, _) ->
        (make_suffix t (fst (Obj.magic params)))@
        ((make_suffix (TInt "") (Obj.magic (snd (Obj.magic params)).abscissa))@
           (make_suffix (TInt "") (Obj.magic (snd (Obj.magic params)).ordinate)))
    | TESuffixs _ -> [Obj.magic params]
    | TAny | TESuffix _ -> (match Obj.magic params with [] -> [""] | p -> p)
    | TESuffixu (_, of_string, string_of) -> [string_of (Obj.magic params)]
    | TJson (_, typ) -> (* server or client side *)
      [ to_json ?typ (Obj.magic params) ]
    | _ -> raise (Eliom_Internal_Error
                    "Bad parameter type in suffix")
  in
  let rec aux typ psuff nlp params pref suff l =
    let open Eliommod_parameters in
    match typ with
    | TNLParams (name, _, _, t) ->
        let psuff, nlp, nl = aux t psuff nlp params pref suff [] in
        (psuff, String.Table.add name nl nlp, l)
    | TProd (t1, t2) ->
        let psuff, nlp, l1 =
          aux t1 psuff nlp (fst (Obj.magic params)) pref suff l
        in
        aux t2 psuff nlp (snd (Obj.magic params)) pref suff l1
    | TOption t ->
        (match ((Obj.magic params) : 'zozo option) with
        | None -> psuff, nlp, l
        | Some v -> aux t psuff nlp v pref suff l)
    | TNEOption t ->
        (match ((Obj.magic params) : 'zozo option) with
        | None -> psuff, nlp, l
        | Some v -> aux t psuff nlp v pref suff l)
    | TBool name ->
        (if ((Obj.magic params) : bool)
         then psuff, nlp, ((pref^name^suff), insert_string "on")::l
         else psuff, nlp, l)
    | TList (list_name, t) ->
        let pref2 = pref^list_name^suff^"." in
        fst
          (List.fold_left
             (fun ((psuff, nlp, s), i) p ->
                ((aux t psuff nlp p pref2 (make_list_suffix i) s), (i+1)))
             ((psuff, nlp, l), 0) (Obj.magic params))
    | TSet t ->
        List.fold_left
          (fun (psuff, nlp, l) v -> aux t psuff nlp v pref suff l)
          (psuff, nlp, l)
          (Obj.magic params)
    | TSum (t1, t2) -> (match Obj.magic params with
      | Inj1 v -> aux t1 psuff nlp v pref suff l
      | Inj2 v -> aux t2 psuff nlp v pref suff l)
    | TString name -> psuff, nlp, ((pref^name^suff),
                                   insert_string (Obj.magic params))::l
    | TInt name ->
        psuff, nlp,
        ((pref^name^suff),
         insert_string (string_of_int (Obj.magic params)))::l
    | TInt32 name ->
        psuff, nlp,
        ((pref^name^suff),
         insert_string (Int32.to_string (Obj.magic params)))::l
    | TInt64 name ->
        psuff, nlp,
        ((pref^name^suff),
         insert_string (Int64.to_string (Obj.magic params)))::l
    | TFloat name ->
        psuff, nlp,
        ((pref^name^suff),
         insert_string (string_of_float (Obj.magic params)))::l
    | TFile name -> psuff, nlp, ((pref^name^suff),
                                 insert_file (Obj.magic params))::l
    | TUserType (name, of_string, string_of) ->
        psuff, nlp,
        ((pref^name^suff),
         insert_string (string_of (Obj.magic params)))::l
    | TTypeFilter (t, check) -> aux t psuff nlp params pref suff l
    | TCoord name ->
        psuff, nlp,
        let coord = Obj.magic params in
        ((pref^name^suff^".x"), insert_string (string_of_int coord.abscissa))::
        ((pref^name^suff^".y"), insert_string (string_of_int coord.ordinate))::l
    | TCoordv (t, name) ->
        aux (TProd (t, TCoord name)) psuff nlp params pref suff l
    | TUnit -> psuff, nlp, l
    | TAny -> psuff, nlp, l@(Obj.magic params)
    | TConst _ -> psuff, nlp, l
    | TESuffix _
    | TESuffixs _
    | TESuffixu _ -> raise (Eliom_Internal_Error "Bad use of suffix")
    | TSuffix (_, s) -> Some (make_suffix s (Obj.magic params)), nlp, l
    | TJson (name, typ) -> (* server or client side *)
      psuff, nlp, ((pref^name^suff),
                   insert_string (to_json ?typ (Obj.magic params)))::l
    | TRaw_post_data ->
      failwith "Constructing an URL with raw POST data not possible"
  in
  aux typ None nlp params "" "" []




(** Given a parameter type, get the two functions
    that converts from and to strings. You should
    only use this function on
    - options ;
    - basic types : int, int32, int64, float, string
    - marshal
    - unit
    - string
    - bool
 *)
let rec get_to_and_from x = match x with
  | TOption o ->
    let (_to, from) = get_to_and_from o in
    Obj.magic
      ((fun s -> try Some (_to s) with _ -> None),
       (function
         | Some alpha -> from alpha
         | None -> ""))
  | TNEOption o ->
    let (_to, from) = get_to_and_from o in
    Obj.magic
      ((fun s -> try Some (_to s) with _ -> None),
       (function
         | Some alpha -> from alpha
         | None -> ""))
  | TUserType (_, _to, from) -> Obj.magic (_to, from)
  | TESuffixu (_, f, g) -> Obj.magic (f, g)
  | TInt32 _ ->
    Obj.magic (Int32.of_string, Int32.to_string)
  | TInt64 _ ->
    Obj.magic (Int64.of_string, Int64.to_string)
  | TFloat _ ->
    Obj.magic (float_of_string, string_of_float)
  | TInt _ ->
    Obj.magic (int_of_string, string_of_int)
  | TBool name -> (* XXX: is that it ? *)
    Obj.magic ((fun s -> print_endline "pop" ; if s = "on" || s = "true" then true else false))
  | TJson (_, typ) -> (* server or client side *)
    Obj.magic ((fun s -> of_json ?typ s),
               (fun d -> to_json ?typ d))
  | TUnit ->
    Obj.magic ((fun _ -> ()), (fun () -> ""))
  | TString _ ->
    Obj.magic ((fun s -> s), (fun s -> s))
  | TNLParams _ | TSuffix _ | TProd (_, _)
  | TList (_, _) | TSet _ | TSum (_, _)
  | TAny | TESuffix _ | TESuffixs _ | TFile _
  | TCoord _ | TCoordv _ | TConst _ | TTypeFilter _
  | TRaw_post_data ->
    failwith "get_to_and_from: not implemented"


let guard construct name guard =
  let (from, _to) = get_to_and_from (construct name) in
  TUserType (name, (fun s -> let alpha = from s in assert (guard alpha); alpha),
             _to)

(** Walk the parameter tree to search for a parameter, given its name *)
let rec walk_parameter_tree name x = match x with
  | TUserType (name', _, _) | TInt name' | TInt32 name' | TInt64 name' | TFile name'
  | TFloat name' | TString name' | TBool name' | TCoord name'
  | TConst name' | TCoordv (_, name') | TESuffix name' | TESuffixs name'
  | TJson (name', _)
  | TESuffixu (name', _, _) ->
    if name = name' then
      Some (get_to_and_from x)
    else
      None
  | TTypeFilter (t, _) -> walk_parameter_tree name t
  | TSuffix ( _, o) -> walk_parameter_tree name o
  | TAny -> None
  | TNLParams _ -> None
  | TUnit -> None
  | (TOption o | TNEOption o | TSet o | TList (_, o)) ->
    Obj.magic (walk_parameter_tree name o)
  | (TProd (a, b) | TSum (a, b)) ->
    (match walk_parameter_tree name a with
      | Some a -> Some a
      | None -> walk_parameter_tree name b)
  | TRaw_post_data ->
    failwith "walk_parameter_tree with raw post data"





(* contruct the string of parameters (& separated) for GET and POST *)
let construct_params_string l =
  Url.make_encoded_parameters (Eliommod_parameters.get_param_list l)

let construct_params_list nonlocparams typ p =
  let (suff, nonlocparams, pl) = construct_params_list_raw nonlocparams typ p in
  let nlp = String.Table.fold (fun _ l s -> l@s) nonlocparams [] in
  let pl = pl@nlp in (* pl at beginning *)
  (suff, pl)


let construct_params nonlocparams typ p =
  let suff, pl = construct_params_list nonlocparams typ p in
  (suff, construct_params_string pl)




let make_params_names (params : ('t,'tipo,'n) params_type) : bool * 'n =
  let rec aux issuffix prefix suffix = function
    | TNLParams (_, _, _, t) -> aux issuffix prefix suffix t
    | TProd (t1, t2) ->
        let issuffix, a = aux issuffix prefix suffix t1 in
        let issuffix, b = aux issuffix prefix suffix t2 in
        issuffix, Obj.magic (a, b)
    | TInt name
    | TInt32 name
    | TInt64 name
    | TFloat name
    | TString name
    | TBool name
    | TFile name
    | TUserType (name, _, _)
    | TCoord name
    | TCoordv (_, name)
    | TJson (name, _)
      -> issuffix, Obj.magic (prefix^name^suffix)
    | TUnit
    | TAny
    | TConst _
      -> issuffix, Obj.magic ()
    | TSet t -> Obj.magic (aux issuffix prefix suffix t)
    | TESuffix n
    | TESuffixs n
    | TESuffixu (n, _, _) -> issuffix, Obj.magic n
    | TSuffix (_, t) -> Obj.magic (aux true prefix suffix t)
    | TOption t -> Obj.magic (aux issuffix prefix suffix t)
    | TNEOption t -> Obj.magic (aux issuffix prefix suffix t)
    | TSum (t1, t2) ->
        let _, a = aux issuffix prefix suffix t1 in
        let _, b = aux issuffix prefix suffix t2 in
        issuffix, Obj.magic (a, b)
        (* TSuffix cannot be inside TSum *)
    | TList (name, t1) ->
        (issuffix,
         Obj.magic
           {it =
               (fun f l init ->
                  let length = List.length l in
                  snd
                    (List.fold_right
                       (fun el (i, l2) ->
                          let i'= i-1 in
                          (i', (f (snd (aux issuffix (prefix^name^suffix^".")
                                          (make_list_suffix i') t1)) el
                                  l2)))
                       l
                       (length, init)))})
          (* TSuffix cannot be inside TList *)
    | TTypeFilter (t, _) -> aux issuffix prefix suffix t
    | TRaw_post_data -> failwith "Not possible with raw post data"
  in aux false "" "" params

let string_of_param_name = id



(* Add a prefix to parameters *)
let rec add_pref_params pref = function
  | TNLParams (a, b, c, t) -> TNLParams (a, b, c, add_pref_params pref t)
  | TProd (t1, t2) -> TProd ((add_pref_params pref t1),
                             (add_pref_params pref t2))
  | TOption t -> TOption (add_pref_params pref t)
  | TNEOption t -> TNEOption (add_pref_params pref t)
  | TBool name -> TBool (pref^name)
  | TList (list_name, t) -> TList (pref^list_name, t)
  | TSet t -> TSet (add_pref_params pref t)
  | TSum (t1, t2) -> TSum ((add_pref_params pref t1),
                           (add_pref_params pref t2))
  | TString name -> TString (pref^name)
  | TInt name -> TInt (pref^name)
  | TInt32 name -> TInt32 (pref^name)
  | TInt64 name -> TInt64 (pref^name)
  | TFloat name -> TFloat (pref^name)
  | TFile name -> TFile (pref^name)
  | TUserType (name, of_string, string_of) ->
      TUserType (pref^name, of_string, string_of)
  | TCoord name -> TCoord (pref^name)
  | TCoordv (t, name) -> TCoordv ((add_pref_params pref t), pref^name)
  | TUnit -> TUnit
  | TAny -> TAny
  | TConst v -> TConst v
  | TESuffix n -> TESuffix n
  | TESuffixs n -> TESuffixs n
  | TESuffixu a -> TESuffixu a
  | TSuffix s -> TSuffix s
  | TJson (name, typ) -> TJson (pref^name, typ)
  | (TTypeFilter _) as a -> a
  | TRaw_post_data -> failwith "Not possible with raw post data"


(*****************************************************************************)
(* Non localized parameters *)

let nl_prod
    (t : ('a, 'su, 'an) params_type)
    (s : ('s, [ `WithoutSuffix ], 'sn) non_localized_params) :
    (('a * 's), 'su, 'an * 'sn) params_type =
  (Obj.magic (TProd (Obj.magic t, Obj.magic (TNLParams s))))


(* removes from nlp set the nlp parameters that are present in param
   specification *)
let rec remove_from_nlp nlp = function
    | TNLParams (n, _, _, _) -> String.Table.remove n nlp
    | TProd (t1, t2) ->
        let nlp = remove_from_nlp nlp t1 in
        remove_from_nlp nlp t2
    | _ -> nlp

type nl_params_set = (string * Eliommod_parameters.param) list String.Table.t

let empty_nl_params_set = String.Table.empty

let add_nl_parameter (s : nl_params_set) t v =
  (fun (_, a, _) -> a) (construct_params_list_raw s (TNLParams t) v)

let table_of_nl_params_set = id

let list_of_nl_params_set nlp = snd (construct_params_list nlp unit ())

let string_of_nl_params_set nlp =
  Url.make_encoded_parameters
    (Eliommod_parameters.get_param_list (list_of_nl_params_set nlp))

let get_nl_params_names t = snd (make_params_names (TNLParams t))

let make_nlp_name persistent prefix name =
  let pr = if persistent then "p_" else "n_" in
  pr^prefix^"-"^name

let make_non_localized_parameters
    ~prefix
    ~name
    ?(persistent = false)
    (p : ('a, [ `WithoutSuffix ], 'b) params_type)
    : ('a, [ `WithoutSuffix ], 'b) non_localized_params =
  let name = make_nlp_name persistent prefix name in
  if String.contains name '.'
  then failwith "Non localized parameters names cannot contain dots."
  else
    (name,
     persistent,
     (Polytables.make_key () (* GET *),
      Polytables.make_key () (* POST *)),
     add_pref_params (Eliom_common.nl_param_prefix^name^".") p)




(*****************************************************************************)
let rec contains_suffix = function
  | TProd (a, b) ->
      (match contains_suffix a with
         | None -> contains_suffix b
         | c -> c)
  | TSuffix (b, _) -> Some b
  | _ -> None


(*****************************************************************************)

let rec wrap_param_type = function
  | TNLParams (a, b, c, t) -> TNLParams (a, b, c, wrap_param_type t)
  | TProd (t1, t2) -> TProd ((wrap_param_type t1),
                             (wrap_param_type t2))
  | TOption t -> TOption (wrap_param_type t)
  | TNEOption t -> TNEOption (wrap_param_type t)
  | TList (list_name, t) -> TList (list_name, t)
  | TSet t -> TSet (wrap_param_type t)
  | TSum (t1, t2) -> TSum ((wrap_param_type t1),
                           (wrap_param_type t2))
  | TUserType (name, of_string, string_of) ->
(*VVV *)
    failwith ("User service parameters '"^name^"' type not supported client side.")
  | TCoordv (t, name) -> TCoordv ((wrap_param_type t), name)
(* We remove the type information here: not possible to send a closure.
   marshaling is just basic json marshaling on client side. *)
  | TJson (name, _) -> TJson (name, None)
  (* the filter is only on server side (at least for now) *)
  | TTypeFilter (t, _) -> TTypeFilter (t, None)
  | t -> t
