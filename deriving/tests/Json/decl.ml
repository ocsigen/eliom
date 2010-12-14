(*pp ${DERIVING} *)

type t_record = {
    v_bool : bool;
    v_unit : unit;
    v_char : char;
    v_string : string;
    v_int : int;
    v_int32 : int32;
    v_int64 : int64;
    (* v_num : num; *)
    v_float : float;
  } deriving (Show,Json)

type t_sum =
  | A
  | B of int
  | C of string * string
  | D of (string * string)
  | E
  | F of t_sum
      deriving (Show,Json)

type 'a t_poly_record = {
    l : 'a. 'a list;
    o : 'a. 'a option;
    loo : 'a. 'a option option list;
    la : 'a list;
  } deriving (Show,Json)

type t_var =
    [ `Ampere | `Bel of int | `Candela of float * int | `Drachm of t_var ]
      deriving (Show,Json)

type t_var' =
    [ t_var | `Erg ] deriving (Show,Json)

(* Module map, première possibilité, un seul "deriver" *)
module IntMap' =  Deriving_Json_stdlib.Map.Make(struct
  type t = int deriving (Json)
  let compare = compare
end)

(* Module map, deuxième possibilité, avec deux "deriver". *)
module IntMap = struct
  module Int = struct type t = int deriving (Show, Json) let compare = compare end
  module M = Map.Make(Int)
  include M
  module Json_t = Deriving_Json_stdlib.Map.MakeJson(Int)
  module Show_t = Deriving_Show_stdlib.Map.MakeShow(Int)
end

type all = (t_record * t_sum list * int t_poly_record option
	      * t_var list * t_var' list
	      * int ref * bool array
	      * float IntMap.t)
      deriving (Show,Json)

let print_all = Show.show<all>

(* Values *)

let v_record = {
    v_bool = true;
    v_unit = ();
    v_char = 'z';
    v_string = "  c'est déjà ça – ∅ ⊢ λx. x : α -> α \r";
    v_int = 3;
    v_int32 = Int32.of_int 4;
    v_int64 = Int64.of_int ~-5;
    (* v_num = Num.num_of_int 6; *)
    v_float = 3.1415;
}

let v_sum_list = [A; B 1; C ("a","b"); D ("c","d"); E ; F (F E)]

let v_poly = {
  l = [];
  o = None;
  loo = [None; Some None; None];
  la = [1;2;3]
}

let v_var_list = [ `Ampere ; `Bel 5 ; `Candela (3.13, 7) ; `Drachm (`Drachm (`Ampere)) ]
let v_var'_list = `Erg :: v_var_list

let v_intMap = List.fold_right2 IntMap.add [1;3;5;7] [1.33;3.66;5.;7.33] IntMap.empty

let all : all = v_record, v_sum_list, Some v_poly, v_var_list,
  v_var'_list, ref 4, [|true;false;true;false;true;false |], v_intMap

let extract_string (v,_,_,_,_,_,_,_) = v.v_string
