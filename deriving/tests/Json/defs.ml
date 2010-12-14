(*pp $DERIVING *)

(* sums (nullary, unary, and n-ary) *)
type sum = S0 | S1 of int | S2 of int * float | S3 of int * float * bool | Sunit of unit | Stup of (int * float) | Stup1 of (int)
  deriving (Json)

type nullsum = N0 | N1 | N2 | N3
    deriving (Json)

(* records with mutable and immutable fields (and various combinations) *)
type r1 = {
  r1_l1 : int;
  r1_l2 : int;
} deriving (Json)

(* type r2 = { *)
  (* mutable r2_l1 : int; *)
  (* mutable r2_l2 : int; *)
(* } deriving (Json) *)

(* type r3 = { *)
  (* r3_l1 : int; *)
  (* mutable r3_l2 : int; *)
(* } deriving (Json) *)

(* polymorphic records *)
type r4 = {
  r4_l1 : 'a . 'a list
} deriving (Json)

(* label types *)
type label = x:int -> int
  (*  deriving (Json) *)

(* function types  *)
type funct = int -> int
  (* deriving (Json) *)

(* recursive types *)
type intseq = INil | ICons of int * intseq
  deriving (Json)

type 'a seq = Nil | Cons of 'a * 'a seq
  deriving (Json)

(* applied type constructors (nullary, n-ary) *)
type uses_seqs = (intseq * float seq)
    deriving (Json)

(* object and class types *)
type obj = < x : int >

(* class types *)
class c = object end

(* polymorphic variants (nullary, unary tags, extending complex type expressions, defined inline) *)
type poly0 = [`T0 | `T1 | `T2 | `T3]
    deriving (Json)

type poly1 = [`T0 | `T1 of int]
    deriving (Json)

type poly2 = P of int * [`T0 | `T1 of int] * float
    deriving (Json)

(* `as'-recursion *)
type poly3 = [`Nil | `Cons of int * 'c] as 'c
    deriving (Json)

type poly3b = int * ([`Nil | `Cons of int * 'c] as 'c) * [`F]
    deriving (Json)

(* <, >, =, > < polymorphic variants *)
type 'a poly7 = Foo of [`F of 'a]
and 'a poly8 = { x : [`G of [`H of [`I of 'a poly7]]] }
    deriving (Json)

(*
type poly9 = [`F | [`G]]
    deriving (Json)
  currently broken.
*)
type poly10 = [`F | poly3]
    deriving (Json)

(* mutually recursive types (monomorphic, polymorphic) *)
type mutrec_a = mutrec_c
and mutrec_b = { l1 : mutrec_c ; l2 : mutrec_a }
and mutrec_c = S of int * mutrec_a | N
and mutrec_d = [`T of mutrec_b]
    deriving (Json)

type ('a,'b) pmutrec_a = ('a,'b) pmutrec_c
and ('a,'b) pmutrec_b = { pl1 : ('a,'b) pmutrec_c ; pl2 : ('a,'b) pmutrec_a }
and ('a,'b) pmutrec_c = SS of 'a * ('a,'b) pmutrec_a * 'b
and ('a,'b) pmutrec_d = [`T of ('a,'b) pmutrec_b]
    deriving (Json)

(* polymorphic types *)
type 'a ff1 = F of 'a * 'a | G of int deriving (Json)
type ('a,'b) ff2 = F1 of ('a,'b) ff2 | F2 of 'a seq * int * 'b option
  deriving (Json)

(* tuples *)
type tup0 = unit
    deriving (Json)
type tup2 = int * float
    deriving (Json)
type tup3 = int * float * bool
    deriving (Json)
type tup4 = int * int * bool * unit
    deriving (Json)

(* type equations (replication) *)
(* TODO *)

(* references *)
type withref = WR of int * (int ref)
  deriving (Json)

(* through module boundaries *)
module rec M : sig
  type t deriving (Json)
end =
struct
  type t = [`N|`C of t] deriving (Json)
end

(* parameterized types through module boundaries *)
module rec P : sig
  type 'a t deriving (Json)
end =
struct
  type 'a t = [`N|`C of 'a t] deriving (Json)
end

(* with constraints *)
type 'a constrained = [`F of 'a] constraint 'a = int
    (* deriving (Json) (\* Show, etc. don't work here *\) *)

(* private datatypes *)
(* type p1 = private P1 *)
    (* deriving (Json) *)

(* check that `private' in the interface is allowed for classes that
   disallow `private' (e.g. Dump) as long as we don't have `private'
   in the implementation *)
module Private : sig
  type p2 = private Q deriving (Json)
end =
struct
  type p2 = Q deriving (Json)
end

(* Reusing existing instances *)
type t = int
    deriving (Json)
