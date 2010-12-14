(*pp $DERIVING *)

open Defs
open Deriving_Pickle
open Deriving_Eq

module Test =
struct
  let test (type t) (m: (module Pickle with type a = t)) v =
    let module P = (val m : Pickle with type a = t) in
    let module Eq = (val P.eq : Eq with type a = t) in
    Eq.eq (P.from_string (P.to_string v)) v
end

let sum =
  begin
    let test = Test.test Pickle_sum.make in
    assert (test S0);
    assert (test (S1 3));
    assert (test (S2 (10,2.0)));
    assert (test (Sunit ()));
    assert (test (Stup (10,2.0)));
    assert (test (Stup1 3));
  end

let nullsum =
  begin
    let test = Test.test Pickle_nullsum.make in
    assert (test N0);
    assert (test N1);
    assert (test N2);
    assert (test N3);
  end

let r1 =
  begin
    let test = Test.test Pickle_r1.make in
    assert (test {r1_l1 = 10; r1_l2 = 20});
    assert (test {r1_l1 = min_int; r1_l2 = max_int});
    assert (test {r1_l1 = max_int; r1_l2 = min_int});
  end

let r2 =
  begin
    let v = { r2_l1 = 10;
              r2_l2 = 14 } in
    assert (not (Eq.eq<r2>
                 (Pickle.from_string<r2>
                  (Pickle.to_string<r2> v)) v));
    assert (Pickle.from_string<r2>
            (Pickle.to_string<r2> v) = v);
  end

let r3 =
  begin
    let v = { r3_l1 = 10;
              r3_l2 = 14 } in
    assert (not (Eq.eq<r3>
                 (Pickle.from_string<r3>
                  (Pickle.to_string<r3> v)) v));
    assert (Pickle.from_string<r3>
            (Pickle.to_string<r3> v) = v);
  end

let r4 =
  begin
    let v = { r4_l1 = [] } in
    assert (Eq.eq<r4>
                 (Pickle.from_string<r4>
                  (Pickle.to_string<r4> v)) v);
    assert (Pickle.from_string<r4>
            (Pickle.to_string<r4> v) = v);
  end

let intseq =
  begin
    let test = Test.test Pickle_intseq.make in
    assert (test INil);
    assert (test (ICons (10, ICons (20, ICons (30, ICons (40, INil))))));
    assert (test (ICons (max_int, ICons (min_int, INil))));
  end

let seq =
  begin
    let test = Test.test (Pickle_seq.make Pickle_bool.make) in
    let test' = Test.test (Pickle_seq.make (Pickle_seq.make Pickle_bool.make)) in
    assert (test Nil);
    assert (test (Cons (false, Cons (true, Cons (false, Nil)))));
    assert (test' Nil);
    assert (test' (Cons (Cons (false, Cons (true, Nil)),
                         Cons (Cons (true, Cons (false, Nil)),
                               Nil))));
  end

let uses_seqs =
  begin
    let test = Test.test Pickle_uses_seqs.make in
      assert (test (INil, Nil));
      assert (test (INil, Cons (0.0, Cons(10.0, Nil))));
      assert (test (ICons (10, ICons(20, INil)), Nil));
      assert (test (ICons (10, ICons(20, INil)),
                    Cons (0.0, Cons(10.0, Nil))));
  end

type permute0 = [`T3 | `T1 | `T2 | `T0] deriving (Typeable, Eq, Pickle)
let poly0 =
  begin
    let test v = Eq.eq<permute0> (Pickle.from_string<permute0> (Pickle.to_string<poly0> v)) v in
    assert (test `T0);
    assert (test `T1);
    assert (test `T2);
    assert (test `T3);
  end

type permute3 = [`Nil | `Cons of int * permute3] deriving (Typeable, Eq, Pickle)
let _ =
  begin
    let test v = Eq.eq<permute3> (Pickle.from_string<permute3> (Pickle.to_string<poly3> v)) v in
    assert (test `Nil);
    assert (test (`Cons (0, `Cons (1, `Cons (2, `Nil)))));
  end

let poly3b =
  begin
    let test = Test.test Pickle_poly3b.make in
    assert (test (10, `Nil, `F));
    assert (test (10, `Cons (10, `Cons (-20, `Nil)), `F));
  end

let _ =
  begin
    let test = Test.test  (Pickle_poly7.make Pickle_bool.make)
    and test' = Test.test (Pickle_poly8.make Pickle_int.make) in
    assert (test (Foo (`F true)));
    assert (test (Foo (`F false)));
    assert (test' {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
    assert (test' {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
  end

let _ =
  begin
    let test = Test.test Pickle_poly10.make in
    assert (test `F);
    assert (test `Nil);
    assert (test (`Cons (12, `Cons (14, `Nil))));
  end

let mutrec =
  begin
    let testA = Test.test Pickle_mutrec_a.make in
    let testB = Test.test Pickle_mutrec_b.make in
    let testC = Test.test Pickle_mutrec_c.make in
    let testD = Test.test Pickle_mutrec_d.make in
    let a = N in
    let b = { l1 = S (3, a); l2 = a } in
    let c = S (3, S (4, S (5, N))) in
    let d = `T b in
    assert (testA a);
    assert (testB b);
    assert (testC c);
    assert (testD d);
  end

let pmutrec = begin
  let testA = Test.test (Pickle_pmutrec_a.make Pickle_int.make Pickle_char.make) in
  let testB = Test.test (Pickle_pmutrec_b.make Pickle_int.make Pickle_char.make) in
  let testC = Test.test (Pickle_pmutrec_c.make Pickle_int.make Pickle_char.make) in
  let testD = Test.test (Pickle_pmutrec_d.make Pickle_int.make Pickle_char.make) in
  let a = NN in
  let b = { pl1 = SS (3, a, 'c'); pl2 = a } in
  let c = SS (3, SS (4, SS (5, NN, 'e'), 'd') ,'c') in
  let d = `T b in
  assert (testA a);
  assert (testB b);
  assert (testC c);
  assert (testD d);
end

let pmutrec' = begin
  let testA = Test.test (Pickle_pmutrec_a'.make Pickle_int.make) in
  let testB = Test.test (Pickle_pmutrec_b'.make Pickle_int.make Pickle_char.make) in
  let testC =
    Test.test (Pickle_pmutrec_c'.make Pickle_char.make Pickle_int.make Pickle_bool.make) in
  let testD = Test.test Pickle_pmutrec_d'.make in
  let a : int pmutrec_a' = SSS (1, NNN, 2) in
  let b : (int, char) pmutrec_b' = { pl1' = SSS ('c', a, 'd');
				     pl2' = SSS ('a', NNN, 'b') } in
  let c : (char, int, bool) pmutrec_c' =
    TTT ('a', SSS (false, SSS (5, NNN, 6), 'b') , true) in
  let d = `T b in
  assert (testA a);
  assert (testB b);
  assert (testC c);
  assert (testD d);
end

let ff1 =
  begin
    let test = Test.test (Pickle_ff1.make Pickle_bool.make) in
    assert (test (F (true,false)));
    assert (test (G 435));
  end

let ff2 =
  begin
    let test = Test.test (Pickle_ff2.make Pickle_bool.make Pickle_int.make) in
    assert (test (F1 (F2 (Nil, 10, None))));
    assert (test (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
  end

let unit =
  begin
    let test = Test.test Pickle_unit.make in
    assert (test ());
  end

let tup2 =
  begin
    let test = Test.test Pickle_tup2.make in
    assert (test (-10,12e4));
    assert (test (max_int,12e4));
  end

let tup3 =
  begin
    let test = Test.test Pickle_tup3.make in
    assert (test (0,12.3,true));
    assert (test (min_int,-12.3,false));
  end

let tup4 =
  begin
    let test = Test.test Pickle_tup4.make in
    assert (test (0,0,true,()));
    assert (test (min_int,max_int,false,()));
  end

let withref =
  begin
    let v = WR (10, ref 20) in
    assert (not (Eq.eq
		<withref>   (Pickle.from_string<withref>
                    (Pickle.to_string<withref> v)) v));
    assert (Pickle.from_string<withref>
            (Pickle.to_string<withref> v) = v);
  end

let t =
  begin
    let test v = Eq.eq<int> (Pickle.from_string<int> (Pickle.to_string<t> v)) v in
    assert (test min_int);
    assert (test max_int);
    assert (test 10);
 end

type refobj = A | B of refobj ref
  deriving (Eq, Typeable, Pickle)

let circular =
  let s = ref A in
  let r = B s in
     s := r;
    r

let _ =
  let v = Pickle.from_string<refobj> (Pickle.to_string<refobj> circular) in
  match v with
  | (B {contents =
           B {contents =
               B {contents =
                   B {contents =
                       B {contents =
                           B {contents =
                               B {contents = _ }}}}}}}) -> ()
  | _ -> assert false

type mut = {
  mutable x : mut option;
  mutable y : mut option;
  z : int;
} deriving (Eq, Typeable, Pickle)

let circularm =
  let i = {z = 1; x = None; y = None} in
  let j = {z = 2; x = None; y = Some i} in
    i.x <- Some j;
    i.y <- Some i;
    j.x <- Some j;
    i

let _ =
  let v = Pickle.from_string<mut> (Pickle.to_string<mut> circularm) in
  match v with
  | {z = 1;
       x = Some {z = 2; x = Some {z = 2;
                                  x = Some _;
                                  y = Some _};
                 y = Some _};
       y = Some {z = 1;
                 x = Some {z = 2; x = Some {z = 2;
                                            x = Some {z = 2;
                                                      x = Some _;
                                                      y = Some _};
                                            y = Some _};
                       y = Some _};
                 y = Some _}} -> ()
  | _ -> assert false

type t1 = { mutable x : t2 option }
and  t2 = { y : t1 option }
    deriving (Eq, Typeable, Pickle)

let circular_a =
  let a = { x = None } in
  let b = { y = Some a } in
    a.x <- Some b;
    a

let _ =
  match Pickle.from_string<t1> (Pickle.to_string<t1> circular_a) with
  | {x = Some {y = Some
         {x = Some {y = Some
              {x = Some {y = Some
                   {x = Some {y = Some _}}}}}}}} -> ()
  | _ -> assert false
