(*pp $DERIVING *)

open Deriving_Typeable

let eq_types t1 t2 = TypeRep.eq (Lazy.force t1) (Lazy.force t2)

type t1 = F deriving (Typeable)
type t2 = F deriving (Typeable)

let _ =
  begin
    assert (eq_types
	      Typeable.type_rep<t1>
              Typeable.type_rep<t1>);
    assert (eq_types
              Typeable.type_rep<t2>
              Typeable.type_rep<t2>);
    assert (not (eq_types
                   Typeable.type_rep<t1>
                   Typeable.type_rep<t2>));
    assert (not (eq_types
                   Typeable.type_rep<t2>
                   Typeable.type_rep<t1>));
  end

type t3 = int deriving (Typeable)

let _ =
  begin
    assert (eq_types
              Typeable.type_rep<int>
              Typeable.type_rep<t3>);
  end


type t4 = [`T of int] deriving (Typeable)
type t5 = [`T of t3] deriving (Typeable)

let _ =
  begin
    assert (eq_types
              Typeable.type_rep<t4>
              Typeable.type_rep<t5>);
  end

type t6 = [`T of t5]
    deriving (Typeable)

let _ =
  begin
    assert (not (eq_types
                   Typeable.type_rep<t5>
                   Typeable.type_rep<t6>));

  end

type t7 = [`T of t6]
    deriving (Typeable)

let _ =
  begin
    assert (not (eq_types
                   Typeable.type_rep<t6>
                   Typeable.type_rep<t7>));
  end


type t8 = [`A | `B] deriving (Typeable)
type t9 = [`B | `A] deriving (Typeable)

let _ =
  begin
    assert (eq_types
              Typeable.type_rep<t8>
              Typeable.type_rep<t9>);
  end


type ('a,'r) openr = [`Nil | `Cons of 'a * 'r]
 deriving (Typeable)
type 'a closedr = [`Nil | `Cons of 'a * 'a closedr]
 deriving (Typeable)
type l1 = [ `A of (int, l1) openr ]
and l2 = [ `A of int closedr ] deriving (Typeable)

(* The following fail without recursive module : *)
(* type l3 = (int, l3) openr deriving (Typeable) *)

let _ =
  begin
    assert (eq_types
              Typeable.type_rep<l1>
              Typeable.type_rep<l1>);
  end

type nil = [`Nil] deriving (Typeable)
type t10 = [ `A of ([nil| `Cons of int * 'a ] as 'a)] list
    deriving (Typeable)
type t11 = l2 list deriving (Typeable)

let _ =
  begin
    assert
      (eq_types
         Typeable.type_rep<t10>
         Typeable.type_rep<t11>);
  end
