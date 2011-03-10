type obj_table

external get_obj_table_uuid : Obj.t -> int = "caml_get_obj_table_uuid"

module Mark : sig
  type t
end
=
struct
  type t = string
end

type mark = Mark.t

(* XXX must be the same as in Ocsigen_wrap *)
type unwrap_id = int

let id_of_int x = x

type unwrapper =
    { id : unwrap_id;
      mutable umark : Mark.t; }

let unwrap_table : (int,Obj.t -> Obj.t) Hashtbl.t = Hashtbl.create 0
(* table containing all the unwrapping functions referenced by their id *)

let register_unwrapper id f = Hashtbl.replace unwrap_table id (fun x -> Obj.repr (f (Obj.obj x)))

let ( -- ) x y =
  let rec aux y x acc =
    if x > y then acc else aux (y-1) x (y::acc)
  in
  aux y x []

let sons_of_obj o =
  if Obj.tag o > Obj.no_scan_tag
  then []
  else List.map (fun i -> Obj.field o i) (0 -- (Obj.size o - 1))

let sons o =
  if Obj.tag o > Obj.no_scan_tag
  then []
  else List.map (fun i -> i,Obj.field o i) (0 -- (Obj.size o - 1))

let rec tail_rec_add t = function
  | [] -> ()
  | v::acc ->
    let acc =
      if Obj.tag v > Obj.no_scan_tag
      then acc
      else
	let uuid = get_obj_table_uuid v in
	(if Hashtbl.mem t uuid
	 then acc
	 else
	    begin
	      Hashtbl.replace t uuid v;
	      (sons_of_obj v)@acc
	    end)
    in
    tail_rec_add t acc

let make_table v =
  let t = Hashtbl.create 0 in
  tail_rec_add t [v];
  t

let find_parents t v =
  let acc = ref [] in
  Hashtbl.iter (fun _ elt -> 
    List.iter
      (fun (i,son) -> if son == v then acc := (i,elt)::!acc)
      (sons elt)) t;
  !acc

(* It's possible to be much more efficient by building a table before reconstructing the value *)
let replace t mark =
  let unwrap (_,unwrapper) =
    let unwrapper' = (Obj.obj unwrapper:unwrapper) in
    let f =
      try
	Hashtbl.find unwrap_table unwrapper'.id
      with
	| Not_found -> failwith ("unregistered unwrapping id: " ^ (string_of_int unwrapper'.id))
    in
    let unwrap_value (_,value) =
      let v = f value in
      List.iter (fun (i,obj) ->
	Obj.set_field obj i v) (find_parents t value)
    in
    List.iter unwrap_value (find_parents t unwrapper)
  in
  List.iter unwrap (find_parents t mark)

let unwrap (mark,v) =
  let v' = ref v in
  (* the root cannot be replaced by the replace function: we add a new root *)
  let t = make_table (Obj.repr v') in
  replace t (Obj.repr mark);
  !(Obj.magic v')

