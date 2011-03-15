(* ocamlfind ocamlopt -linkpkg -package react -g -I ../ ../wrapping.cmxa test_wrapping.ml -o test_wrapping *)
let _ = Printexc.record_backtrace true

(*** simple wrap test ***)

type a =
    { a : float;
      a_wrap : a Ocsigen_wrap.wrapper; }

let a_wrap () = Ocsigen_wrap.create_wrapper 
  (fun t -> { a = t.a +. 1.; a_wrap = Ocsigen_wrap.empty_wrapper })

let a i = { a = i; a_wrap = a_wrap () }

let va = a 3.14

let _,v = Ocsigen_wrap.wrap va
let () = assert (v.a -. 4.14 < 0.0001)

(*** deep wrap test ***)

let va = [[[[[[1,[3.1,a 3.14]],a 35.1]]]]]

let _,_ = Ocsigen_wrap.wrap va

(*** multiple wrap test ***)

type b =
    { b : string;
      ba : a;
      b' : string;
      b_wrap : b Ocsigen_wrap.wrapper; }

let b_wrap () = Ocsigen_wrap.create_wrapper 
  (fun t -> t.b, t.ba )

let b s f = { b = s; b' = s; b_wrap = b_wrap (); ba = a f }

let tst_string = "test"

let vb = b tst_string 3.14

let _,vb' = Ocsigen_wrap.wrap vb

let _ = assert (
  let (t,f) = Obj.magic (vb') in
  t == tst_string && ( f -. 4.14 < 0.0001 ))

let _,vb'' = Ocsigen_wrap.wrap [2,[1.,[vb,4,ref 0],ref 42]]

let () =
  match vb'' with
    | [2,[1.,[vb',4,{ contents = 0}],{ contents = 42}]] ->
      assert (
	let (t,f) = Obj.magic (vb') in
	t == tst_string && ( f -. 4.14 < 0.0001 ))
    | _ -> assert false


(*** create wrap during wrap test ***)

let b'_wrap () = Ocsigen_wrap.create_wrapper 
  (fun t -> t.b, a 1.2 )

let b' s f = { b = s; b' = s; b_wrap = b'_wrap (); ba = a f }

let vb' = b' "test" 3.14

let _,vb'' = Ocsigen_wrap.wrap vb'

let () =
  match (Obj.magic vb'') with
    | (x,y) -> 
      assert (x == vb'.b);
      assert (y.a -. 4.14 < 0.0001 )


(*** big value copy ***)

let ( -- ) x y =
  let rec aux y x acc =
    if x > y then acc else aux (y-1) x (y::acc)
  in
  aux y x []
(*
type l =
    | A
    | L of l * int

let ( -- ) x y =
  let rec aux y x acc =
    if x > y then acc else aux (y-1) x (L (acc,y))
  in
  aux y x A

let _ = Marshal.to_string (0--50000000)
*)
let v = (0--80000)
(* it cannot grow much bigger than that, On systems with a small
   stack, it could die with stack overflow *)
let _,v' = Ocsigen_wrap.wrap v

let () = assert (v' = v)

(*** simple wrap weak test ***)

let d = Weak.create 1
let d_val = (Some (ref 0))
let _ = Weak.set d 0 d_val

type d =
    { da : int;
      dw : int Weak.t;
      dm : d Ocsigen_wrap.wrapper; }

let d_wrap () = Ocsigen_wrap.create_wrapper (fun {da;dw} -> Weak.get dw 0,da)

let _,d' = Ocsigen_wrap.wrap (1,d,d_wrap ())

let () = assert (Obj.magic d' = (d_val,1))

(*** simple wrap react test ***)

let r',push = React.E.create ()
let r = React.E.map (fun i -> i+1) r'

let c a r w = (a,r,w)
let c_wrap () = Ocsigen_wrap.create_wrapper (fun (a,r,w) -> a)

let _,c' = Ocsigen_wrap.wrap (c 1 r (c_wrap ()))
let () = assert (Obj.magic c' = 1)

(*** Eliom_react like test ***)

let r',push = React.E.create ()
let r = React.E.map (fun i -> i+1) r'

type toto =
    { a : float;
      mtoto : toto Ocsigen_wrap.wrapper; }

type t =
    { v1 : int;
      v2 : toto;
      v3 : int React.event;
      mt : t Ocsigen_wrap.wrapper; }

let i = ref 0

let mtoto () = Ocsigen_wrap.create_wrapper (fun t -> incr i; string_of_float t.a, !i)
let mt () = Ocsigen_wrap.create_wrapper (fun t -> incr i; t.v2)

let toto i = { a = i; mtoto = mtoto () }
let t i b = { v1 = i; v2 = b; v3 = r; mt = mt () }

let vtoto = toto 3.14
let vt = t 42 vtoto

let _,v' = Ocsigen_wrap.wrap vtoto
let _,v' = Ocsigen_wrap.wrap vt

(*** closure copy test ***)

type t1 =
    { t1a : float;
      t1mark : t1 Ocsigen_wrap.wrapper; }

type t2 =
    { t2t1 : t1;
      t2f : (int ref -> unit) option;
      t2mark : t2 Ocsigen_wrap.wrapper; }

let r1 = ref 13
let r2 = ref 42
let r3 = ref 88

let t1mark () = Ocsigen_wrap.create_wrapper (fun t -> incr r1; { t1a = 3.14; t1mark = Ocsigen_wrap.empty_wrapper } )
let t2mark () = Ocsigen_wrap.create_wrapper (fun t -> 
  (match t.t2f with
    | Some f -> f r2;
    | None -> assert false);
  { t with t2f = None; t2mark = Ocsigen_wrap.empty_wrapper } )

let t1 = { t1a = 1.1; t1mark = t1mark () }
let t2 = { t2t1 = t1; t2f = Some (fun r -> incr r; incr r3); t2mark = t2mark () }

let _,t2' = Ocsigen_wrap.wrap (Obj.repr t2)
let _,t1' = Ocsigen_wrap.wrap (Obj.repr t1)


let _ =
  assert (!r1 = 15);
  assert (!r2 = 43);
  assert (!r3 = 89)
