(*pp $DERIVING *)

open Defs
open Deriving_Dump

module Test =
  struct
    let test (type t) (m: (module Dump with type a = t)) v =
      let module P = (val m : Dump with type a = t) in
      (P.from_string (P.to_string v)) = v
  end

let sum = begin
  let test = Test.test Dump_sum.make in
  assert (test S0);
  assert (test (S1 max_int));
  assert (test (S2 (min_int, 1243.2)));
  assert (test (S2 (min_int, 1243.2)));
  assert (test (S3 (12, 0.0, true)));
  assert (test (Sunit ()));
  assert (test (Stup (1001, 10.01)));
end

let r1 = begin
  let test = Test.test Dump_r1.make in
  assert (test {r1_l1 = max_int - 10; r1_l2 = min_int + 10});
end

let r4 = begin
  let test = Test.test Dump_r4.make in
  assert (test {r4_l1 = []});
end

let intseq = begin
  let test = Test.test Dump_intseq.make in
  assert (test INil);
  assert (test (ICons (10, ICons (20, ICons (30, INil)))));
end

let seq = begin
  let test = Test.test (Dump_seq.make Dump_bool.make) in
  assert (test Nil);
  assert (test (Cons (true, Cons (false, Cons (true, Nil)))));
end

let uses_seqs = begin
  let test = Test.test Dump_uses_seqs.make in
  assert (test (INil, Nil));
  assert (test (INil, Cons (0.0, Cons(10.0, Nil))));
  assert (test (ICons (10, ICons(20, INil)), Nil));
  assert (test (ICons (10, ICons(20, INil)),
                Cons (0.0, Cons(10.0, Nil))));
end

let poly1 = begin
  let test = Test.test Dump_poly1.make in
  assert (test `T0);
  assert (test (`T1 (-1231)));
end

let poly2 = begin
  let test = Test.test Dump_poly2.make in
  assert (test (P (10, `T1 11, 12.0)));
end

let poly3 = begin
  let test = Test.test Dump_poly3.make in
  assert (test `Nil);
  assert (test (`Cons (1, `Cons (2, `Cons (3, `Nil)))));
end

let poly3b = begin
  let test = Test.test Dump_poly3b.make in
  assert (test (10, `Nil, `F));
  assert (test (0, `Cons (10, `Cons (11, `Cons (12, `Nil))), `F));
end

let poly7 = begin
  let test  = Test.test (Dump_poly7.make Dump_bool.make) in
  let test' = Test.test (Dump_poly8.make Dump_int.make)  in
  assert (test (Foo (`F true)));
  assert (test (Foo (`F false)));
  assert (test' {x = `G (`H (`I (Foo (`F (max_int - 1)))))});
  assert (test' {x = `G (`H (`I (Foo (`F (min_int + 1)))))});
end

let poly10 = begin
  let test = Test.test Dump_poly10.make in
  assert (test `F);
  assert (test `Nil);
  assert (test (`Cons (12, `Cons (14, `Nil))));
end

let mutrec = begin
  let testA = Test.test Dump_mutrec_a.make in
  let testB = Test.test Dump_mutrec_b.make in
  let testC = Test.test Dump_mutrec_c.make in
  let testD = Test.test Dump_mutrec_d.make in
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
  let testA = Test.test (Dump_pmutrec_a.make Dump_int.make Dump_char.make) in
  let testB = Test.test (Dump_pmutrec_b.make Dump_int.make Dump_char.make) in
  let testC = Test.test (Dump_pmutrec_c.make Dump_int.make Dump_char.make) in
  let testD = Test.test (Dump_pmutrec_d.make Dump_int.make Dump_char.make) in
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
  let testA = Test.test (Dump_pmutrec_a'.make Dump_int.make) in
  let testB = Test.test (Dump_pmutrec_b'.make Dump_int.make Dump_char.make) in
  let testC =
    Test.test (Dump_pmutrec_c'.make Dump_char.make Dump_int.make Dump_bool.make) in
  let testD = Test.test Dump_pmutrec_d'.make in
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

let ff1 = begin
  let test = Test.test (Dump_ff1.make Dump_bool.make) in
  assert (test (F (true,false)));
  assert (test (G 435));
end

let ff2 = begin
  let test = Test.test (Dump_ff2.make Dump_bool.make Dump_int.make) in
  assert (test (F1 (F2 (Nil, 10, None))));
  assert (test (F1 (F2 (Cons (true, Cons (false, Nil)), 10, Some 14))));
end

let tup0 = begin
  let test = Test.test Dump_tup0.make in
  assert (test ());
end

let tup2 = begin
  let test = Test.test Dump_tup2.make in
  assert (test (10, 10.0));
  assert (test (max_int, -10.0));
end

let tup3 = begin
  let test = Test.test Dump_tup3.make in
  assert (test (0,12.3,true));
  assert (test (min_int,-12.3,false));
end

let tup4 = begin
  let test = Test.test Dump_tup4.make in
  assert (test (0,0,true,()));
  assert (test (min_int,max_int,false,()));
end

let t = begin
  let test = Test.test Dump_t.make in
  assert (test min_int);
  assert (test max_int);
  assert (test 10);
end
