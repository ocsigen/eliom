#load "str.cma"

let space_re = Str.regexp " +"
let edges = Hashtbl.create 128
let edge_count = Hashtbl.create 128

let chop s =
  try
    let i = String.rindex s '.' in
    String.sub s 0 i
  with Not_found -> s

let add_edge target dep =
  if target <> dep
  then (
    Hashtbl.replace edges dep
      (target :: (try Hashtbl.find edges dep with Not_found -> []));
    Hashtbl.replace edge_count target
      (1 + try Hashtbl.find edge_count target with Not_found -> 0);
    if not (Hashtbl.mem edge_count dep) then Hashtbl.add edge_count dep 0)

let sort l =
  let res = ref [] in
  List.iter
    (fun (target, deps) ->
      let target = chop target in
      if not (Hashtbl.mem edge_count target)
      then Hashtbl.add edge_count target 0;
      List.iter (fun dep -> add_edge target (chop dep)) deps)
    l;
  let q = Queue.create () in
  Hashtbl.iter
    (fun target count -> if count = 0 then Queue.add target q)
    edge_count;
  while not (Queue.is_empty q) do
    let n = Queue.take q in
    res := n :: !res;
    let l = try Hashtbl.find edges n with Not_found -> [] in
    Hashtbl.remove edges n;
    List.iter
      (fun target ->
        let c = Hashtbl.find edge_count target - 1 in
        Hashtbl.replace edge_count target c;
        if c = 0 then Queue.add target q)
      l
  done;
  if Hashtbl.length edges <> 0
  then (
    Format.eprintf "Dependency loop!@.";
    exit 1);
  List.rev !res

let _ =
  let ch = open_in Sys.argv.(1) in
  let lst = ref [] in
  (try
     while true do
       let l = input_line ch in
       let l = Str.split space_re l in
       match l with
       | target :: ":" :: deps -> lst := (target, deps) :: !lst
       | _ -> assert false
     done
   with End_of_file -> ());
  let lst = sort !lst in
  let files = Hashtbl.create 128 in
  for i = 2 to Array.length Sys.argv - 1 do
    Hashtbl.add files (chop Sys.argv.(i)) Sys.argv.(i)
  done;
  List.iter
    (fun f ->
      try Format.printf "%s@." (Hashtbl.find files f) with Not_found -> ())
    lst
