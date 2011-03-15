
let new_id = let r = ref 0 in fun () -> incr r; !r

class type obj_with_id = object
  method camlObjTableId : int Js.optdef Js.prop
end

let get_obj_table_uuid o =
  let v : obj_with_id Js.t = Obj.magic o in
  match Js.Optdef.to_option ( v##camlObjTableId ) with
    | None ->
      let id = new_id () in
      v##camlObjTableId <- Js.def id;
      id
    | Some id -> id

module IdType =
struct
  type t = Obj.t
  let hash v =
    let v = Obj.repr v in
    let tag = Obj.tag v in
    if tag < Obj.no_scan_tag
    then get_obj_table_uuid v
    else 
      (Firebug.console##error_2(Js.string "can't give id to value",v);
       failwith ("can't give id to value"))
  let equal = (==)
end

module T = Hashtbl.Make(IdType)

module Mark : sig
  type t
end
=
struct
  type t = string
end

(* XXX must be the same as in Ocsigen_wrap *)
type unwrap_id = int

let id_of_int x = x

type unwrapper =
    { id : unwrap_id;
      mutable umark : Mark.t; }

let unwrap_table : (int,Obj.t -> Obj.t) Hashtbl.t = Hashtbl.create 0
(* table containing all the unwrapping functions referenced by their id *)

let register_unwrapper id f =
  Hashtbl.replace unwrap_table id (fun x -> Obj.repr (f (Obj.obj x)))

let apply_unwrapper unwrapper v =
  let f =
    try
      Hashtbl.find unwrap_table unwrapper.id
    with
      | Not_found -> failwith ("unregistered unwrapping id: " ^ (string_of_int unwrapper.id))
  in
  f v

let is_marked (mark:Mark.t) o =

  let is_mark o =
    if (Obj.tag o = 0 && Obj.size o = 2 && Obj.field o 1 == (Obj.repr mark))
    then (let id = (Obj.field o 0) in
	  assert (Obj.tag id = Obj.int_tag);
	  true)
    else false
  in

  if (Obj.tag o = 0 && Obj.size o >= 2)
  (* WARNING: we only allow block values with tag = 0 to be wrapped.
     It is easier: we do not have to do another test to know if the
     value is a function *)
  then
    begin
      let potential_mark = (Obj.field o (Obj.size o - 1)) in
      if is_mark potential_mark
      then Some (Obj.obj potential_mark:unwrapper)
      else None
    end
  else None

let rec search_and_replace_ mark t v =
  let tag = Obj.tag v in
  try
    if (not (Obj.is_int (Obj.repr tag))) || tag >= Obj.no_scan_tag || tag = Obj.closure_tag
    || tag = Obj.infix_tag || tag = Obj.lazy_tag || tag = Obj.object_tag
    then v
    else T.find t v
  with
    | Not_found ->
      match is_marked mark v with
	| Some unwrapper ->
	  let size = Obj.size v in
	  for i = 0 to size - 2 do
	    Obj.set_field v i (search_and_replace_ mark t (Obj.field v i));
	  done;
	  let new_v = apply_unwrapper unwrapper v in
	  let res = search_and_replace_ mark t new_v in
	  T.replace t v res;
	  res
	| None ->
	  begin
	    let size = Obj.size v in
	    T.add t v v;
	    (* It is ok to do this because tag < no_scan_tag and it is
	       not a closure ( either infix, normal or lazy ) *)
	    for i = 0 to size - 1 do
	      Obj.set_field v i (search_and_replace_ mark t (Obj.field v i));
	    done;
	    v
	  end

let unwrap (mark,v) =
  let t = T.create 1 in
  Obj.obj (search_and_replace_ (Obj.magic mark) t (Obj.repr v))
