(*pp ${DERIVING} *)

open Defs
open Deriving_Eq

let sum =
  begin
    assert (Eq.eq<sum> S0 S0);
    assert (not (Eq.eq<sum> S0 (S1 0)));
    assert (Eq.eq<sum> (S1 0) (S1 0));
    assert (Eq.eq<sum> (Stup (3,0.0)) (Stup (3,0.0)));
    assert (not (Eq.eq<sum> (Stup (0,0.0)) (Stup (1,0.0))));
  end

let nullsum =
  begin
    assert (Eq.eq<nullsum> N2 N2)
  end

let r1 =
  begin
    assert (Eq.eq
<r1>              { r1_l1 = 10; r1_l2 = 20 }
              { r1_l1 = 10; r1_l2 = 20 });
    assert (not (Eq.eq
<r1>                   { r1_l1 = 20; r1_l2 = 10 }
                   { r1_l1 = 10; r1_l2 = 20 }));
  end

let r2 =
  begin
    let l, r = ({ r2_l1 = 10; r2_l2 = 20},
                { r2_l1 = 10; r2_l2 = 20}) in
    assert (Eq.eq<r2> l l);
    assert (not (Eq.eq<r2> l r));
    assert (not (Eq.eq<r2> r l));
  end

let r3 =
  begin
    let l, r = ({ r3_l1 = 10; r3_l2 = 20},
                { r3_l1 = 10; r3_l2 = 20}) in
    assert (Eq.eq<r3> l l);
    assert (not (Eq.eq<r3> l r));
    assert (not (Eq.eq<r3> r l));
  end

let r4 =
  begin
    let l, r = ({ r4_l1 = []},
                { r4_l1 = []}) in
    assert (Eq.eq<r4> l l);
    assert (Eq.eq<r4> l r);
    assert (Eq.eq<r4> r l);
  end

let intseq =
  begin
    assert (Eq.eq<intseq> INil INil);
    assert (Eq.eq<intseq> (ICons (1,INil)) (ICons (1,INil)));
    assert (not (Eq.eq<intseq> (ICons (1,INil)) INil));
    assert (not (Eq.eq<intseq> INil (ICons (1,INil))));
    assert (not (Eq.eq<intseq> INil (let rec i = ICons(1,i) in i)));
  end

let uses_seqs =
  begin
    let eq = Eq.eq<uses_seqs> in
    assert (eq (INil,Cons(1.0,Nil)) (INil,Cons(1.0,Nil)));
    assert (not (eq (INil,Cons(1.0,Nil)) (INil,Cons(2.0,Nil))));
    assert (not (eq (ICons (1,INil),Nil) (INil,Nil)));
  end

let poly0 =
  begin
    let eq = Eq.eq<poly0> in
      assert (eq `T0 `T0);
      assert (not (eq `T1 `T3));
  end

let poly1 =
  begin
    let eq = Eq.eq<poly1> in
      assert (eq `T0 `T0);
      assert (eq (`T1 10) (`T1 10));
      assert (not (eq (`T1 20) (`T1 10)));
      assert (not (eq (`T1 20) `T0));
  end

let poly2 =
  begin
    let eq = Eq.eq<poly2> in
      assert (eq (P (3, `T0, 0.0)) (P (3, `T0, 0.0)));
      assert (eq (P (4, `T1 10, 2.0)) (P (4, `T1 10, 2.0)));
      assert (not (eq (P (5, `T1 10, 2.0)) (P (5, `T0, 2.0))));
      assert (not (eq (P (6, `T0, 2.0)) (P (6, `T0, 10.0))));
      assert (not (eq (P (0, `T0, 2.0)) (P (7, `T0, 2.0))));
  end


let poly3 =
  begin
    let eq = Eq.eq<poly3> in
      assert (eq `Nil `Nil);
      assert (eq (`Cons (3,`Nil)) (`Cons (3,`Nil)));
      assert (eq (`Cons (3,`Cons (4,`Nil))) (`Cons (3,`Cons (4,`Nil))));
      assert (not (eq (`Cons (3,`Cons (4,`Nil))) (`Cons (3,`Nil))));
  end

let poly3b =
  begin
    let eq = Eq.eq<poly3b> in
    assert (eq (0,`Nil,`F)  (0,`Nil,`F));
    assert (not (eq (0,`Cons (1,`Nil),`F)  (0,`Nil,`F)));
    assert (not (eq (1,`Nil,`F)  (0,`Nil,`F)));
  end


let poly7_8 =
  begin
    let eq7 = Eq.eq<int poly7> in
    let eq8 = Eq.eq<int poly8> in
    assert (eq7 (Foo (`F 0)) (Foo (`F 0)));
    assert (not (eq7 (Foo (`F 0)) (Foo (`F 1))));
    assert (eq8
	      {x = `G (`H (`I (Foo (`F 0))))}
              {x = `G (`H (`I (Foo (`F 0))))});
    assert (not (eq8
                   {x = `G (`H (`I (Foo (`F 0))))}
                   {x = `G (`H (`I (Foo (`F 1))))}));
  end

let poly10 =
  begin
    let eq = Eq.eq<poly10> in
    assert (eq `F `F);
    assert (eq `Nil `Nil);
    assert (not (eq `Nil `F));
  end

let mutrec =
  begin
    let rec cyclic_1 = S (0, cyclic_2)
    and     cyclic_2 = S (1, cyclic_1) in
    assert (not (Eq.eq<mutrec_a> cyclic_1 cyclic_2));
    assert (not (Eq.eq<mutrec_d>
		   (`T {l1 = cyclic_1; l2 = cyclic_2})
                   (`T {l1 = cyclic_2; l2 = cyclic_1})));
  end

let pmutrec =
  begin

    let rec cyclic_1 = SS (0, cyclic_2, true)
    and     cyclic_2 = SS (1, cyclic_1, true) in
      assert (not (Eq.eq<(int, bool) pmutrec_a> cyclic_1 cyclic_2));
      assert (not
                (Eq.eq<(int, bool) pmutrec_d>
                   (`T {pl1 = cyclic_1; pl2 = cyclic_2})
                   (`T {pl1 = cyclic_2; pl2 = cyclic_1})));
  end

let pmutrec' =
  begin

    let rec cyclic_1 = TTT ('a', cyclic_2, 'b')
    and     cyclic_2 = TTT ('b', cyclic_1, 'b') in
    assert (not (Eq.eq<(char, unit, char) pmutrec_c'> cyclic_1 cyclic_2));
    assert (not
              (Eq.eq<pmutrec_d'>
               (`T {pl1' = cyclic_1; pl2' = cyclic_2})
                 (`T {pl1' = cyclic_2; pl2' = cyclic_1})));
  end


let ff1 =
  begin
    let eq = Eq.eq<bool ff1> in
      assert (eq (F (true,false)) (F (true,false)));
      assert (eq (G (-1)) (G (-1)));
      assert (not (eq (F (false,true)) (F (true,false))));
      assert (not (eq (G (-1)) (G 0)));
      assert (not (eq (G (-1)) (F (true, true))));
  end

let ff2 =
  begin
    let eq = Eq.eq<(bool, bool) ff2> in
      assert (eq
                (F1 (F2 (Cons (true,Nil), 0, None)))
                (F1 (F2 (Cons (true,Nil), 0, None))));

      assert (not (eq
                     (F2 (Nil, 0, None))
                     (F2 (Cons (true,Nil), 0, None))));

      assert (not (eq
                     (F2 (Cons (true,Nil), 0, Some true))
                     (F2 (Cons (true,Nil), 0, Some false))));

      assert (not (eq
                     (F2 (Cons (true,Nil), 0, None))
                     (F2 (Cons (true,Nil), 0, Some false))));
  end

let tup0 =
  begin
    assert (Eq.eq<tup0> () ());
  end

let tup2 =
  begin
    assert (Eq.eq<tup2> (10,5.0) (10,5.0));
    assert (not (Eq.eq<tup2> (10,5.0) (11,5.0)));
    assert (not (Eq.eq<tup2> (10,5.0) (10,4.0)));
  end

let tup3 =
  begin
    assert (Eq.eq<tup3> (10,2.5,true) (10,2.5,true));
    assert (not (Eq.eq<tup3> (10,2.5,true) (11,2.5,true)));
    assert (not (Eq.eq<tup3> (10,2.5,true) (10,2.4,true)));
    assert (not (Eq.eq<tup3> (10,2.5,true) (10,2.5,false)));
  end

let tup4 =
  begin
    assert (Eq.eq<tup4> (1,2,true,()) (1,2,true,()));
    assert (not (Eq.eq<tup4> (1,2,true,()) (0,2,true,())));
    assert (not (Eq.eq<tup4> (1,2,true,()) (1,3,true,())));
    assert (not (Eq.eq<tup4> (1,2,true,()) (1,2,false,())));
  end

let withref =
  begin
    let x = ref 0 in
      assert (Eq.eq<withref> (WR (0,x)) (WR (0,x)));
      assert (not (Eq.eq<withref> (WR (0,x)) (WR (0,ref 0))));
  end

let t =
  begin
    assert (Eq.eq<t> 0 0);
    assert (Eq.eq<t> (-10) (-10));
    assert (Eq.eq<t> 14 14);
    assert (not (Eq.eq<t> 14 0));
    assert (not (Eq.eq<t> 0 14));
    assert (not (Eq.eq<t> (-1) 0));
  end
