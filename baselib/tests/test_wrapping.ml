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

let d1,d2 = Ocsigen_wrap.debug_wrap va

(*** deep wrap test ***)

let va = [[[[[[1,[3.1,a 3.14]],a 35.1]]]]]

let d1,d2 = Ocsigen_wrap.debug_wrap va

(*** multiple wrap test ***)

type b =
    { b : string;
      ba : a;
      b_wrap : b Ocsigen_wrap.wrapper;
      b' : string; }

let b_wrap () = Ocsigen_wrap.create_wrapper 
  (fun t -> t.b, t.ba )

let b s f = { b = s; b' = s; b_wrap = b_wrap (); ba = a f }

let vb = b "test" 3.14

let d1,d2 = Ocsigen_wrap.debug_wrap vb

let d1,d2 = Ocsigen_wrap.debug_wrap [2,[1.,[vb,4,ref 0],ref 42]]

(*** create wrap during wrap test ***)

let b'_wrap () = Ocsigen_wrap.create_wrapper 
  (fun t -> t.b, a 1.2 )

let b' s f = { b = s; b' = s; b_wrap = b'_wrap (); ba = a f }

let vb' = b' "test" 3.14

let d1,d2 = Ocsigen_wrap.debug_wrap vb'

(*** big value copy ***)

let ( -- ) x y =
  let rec aux y x acc =
    if x > y then acc else aux (y-1) x (y::acc)
  in
  aux y x []

let _ =
  try
    let t = Obj_table.make (Obj.repr (1 -- 10000)) in
    let t' = Obj_table.copy t in
    let t'' = Obj_table.copy t' in
    let _ = Obj_table.debug t'' in
    ()
  with
    | e -> Gc.print_stat stdout; raise e 

(*** copy react test ***)

let check_table t =
  Obj_table.iter t (fun v entry ->
    assert (v == entry.Obj_table.v);
    assert (List.length (Obj_table.sons v) < 1000); (* reasonable in our case *)
  )

let r',push = React.E.create ()
let r = React.E.map (fun i -> i+1) r'

let () =
  try
    let o = Obj.repr (1,r,1.) in
    let t = Ocsigen_wrap.make_table o in
    ignore (Obj_table.debug t);
    let t' = Obj_table.copy t in
    ignore (Obj_table.debug t');
    ()
  with
    | e -> Gc.print_stat stdout; raise e 

(*** simple wrap weak test ***)

let d = Weak.create 1
let _ = Weak.set d 0 (Some (ref 0))

let d_wrap () = Ocsigen_wrap.create_wrapper (fun (a,d,w) -> Weak.get d 0,a)

let d1,d2 = Ocsigen_wrap.debug_wrap (1,d,d_wrap ())

(*** simple wrap react test ***)

let r',push = React.E.create ()
let r = React.E.map (fun i -> i+1) r'

let c a r w = (a,r,w)
let c_wrap () = Ocsigen_wrap.create_wrapper (fun (a,r,w) -> a)

let d1,d2 = Ocsigen_wrap.debug_wrap (c 1 r (c_wrap ()))

(*** Eliom_react like test ***)

let r',push = React.E.create ()
let r = React.E.map (fun i -> i+1) r'

let d1,d2 = Ocsigen_wrap.debug_wrap (r)


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

let _ = Obj_table.debug (Obj_table.make (Obj.repr vt))

let d1,d2 = Ocsigen_wrap.debug_wrap r
let d1,d2 = Ocsigen_wrap.debug_wrap vtoto
let d1,d2 = Ocsigen_wrap.debug_wrap vt

(*** hidden float array test ***)

let v' = (Obj.repr (1,3.1))
let v = (Obj.repr (3.1,1))

let _ = Obj_table.debug (Obj_table.make v')
let _ = Obj_table.debug (Obj_table.make v)

(*** bad traversal test ***)

let o = Obj.repr (1,(2,3))
let t = Obj_table.make o
let a,b = Obj.field o 0, Obj.field o 1
let c,d = Obj.field b 0, Obj.field b 1
let _ = assert (Obj_table.mem t o)
let _ = assert (Obj_table.mem t b)
