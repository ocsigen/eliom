open Eio.Std

(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. 
   
   It is a (partial) translation of Lwt_stream to Eio
*)

exception Closed
exception Full
exception Empty
exception Cancelled

(* A node in a queue of pending data. *)
type 'a node =
  { mutable next : 'a node
  ; (* Next node in the queue. For the last node it points to itself. *)
    mutable data : 'a option
    (* Data of this node. For the last node it is always [None]. *) }

(* Note: a queue for an exhausted stream is represented by a node
   containing [None] followed by a node with itself as next and [None]
   as data. *)

let new_node () =
  let rec node = {next = node; data = None} in
  node

(* Type of a stream source using a function to create new elements. *)
type 'a from =
  { from_create : unit -> 'a option
  ; (* Function used to create new elements. *)
    mutable from_promise : unit Eio.Promise.or_exn
    (* Promise that is currently being computed:

     - wait for the thread returned by the last call to [from_next],
     - add the next element to the end of the queue.

     If it is not resolved, then it must be used instead of creating a
     new one with [from_create]. *)
  }

(* Type of a stream source for push streams. *)
[@@@ocaml.warning "-69"]

type push =
  { mutable push_signal : unit Eio.Promise.or_exn
  ; (* Promise resolved when a new element is added to the stream. *)
    mutable push_waiting : bool
  ; (* Is a thread waiting on [push_signal] ? *)
    mutable push_external : Obj.t [@ocaml.warning "-69"]
    (* Reference to an external source. *) }

(* Type of a stream source for bounded-push streams. *)
type 'a push_bounded =
  { mutable pushb_signal : unit Eio.Promise.or_exn
  ; (* Thread signaled when a new element is added to the stream. *)
    mutable pushb_waiting : bool
  ; (* Is a thread waiting on [pushb_signal] ? *)
    mutable pushb_size : int
  ; (* Size of the queue. *)
    mutable pushb_count : int
  ; (* Current length of the queue. *)
    mutable pushb_pending : 'a option
  ; (* The next element to push if a thread blocked on push. We store it
     here to be sure it will be the first element to be added when
     space becomes available. *)
    mutable pushb_push_waiter : unit Eio.Promise.or_exn
  ; mutable pushb_push_wakener : (unit, exn) Stdlib.result Eio.Promise.u
  ; (* Thread blocked on push. *)
    mutable pushb_external : Obj.t [@ocaml.warning "-69"]
    (* Reference to an external source. *) }

[@@@ocaml.warning "+69"]

(* Source of a stream. *)
type 'a source =
  | From of 'a from
  | From_direct of (unit -> 'a option)
  | Push of push
  | Push_bounded of 'a push_bounded

type 'a t =
  { source : 'a source
  ; (* The source of the stream. *)
    close : unit Eio.Promise.u
  ; (* A wakener for a thread that sleeps until the stream is closed. *)
    closed : unit Promise.t
  ; (* A waiter for a thread that sleeps until the stream is closed. *)
    mutable node : 'a node
  ; (* Pointer to first pending element, or to [last] if there is no
     pending element. *)
    last : 'a node ref
    (* Node marking the end of the queue of pending elements. *) }

class type ['a] bounded_push = object
  method size : int
  method resize : int -> unit
  method push : 'a -> unit
  method close : unit
  method count : int
  method blocked : bool
  method closed : bool
  method set_reference : 'a. 'a -> unit
end

(* The only difference between two clones is the pointer to the first
   pending element. *)
let clone s =
  (match s.source with
  | Push_bounded _ -> invalid_arg "Eliom_stream.clone"
  | From _ | From_direct _ | Push _ -> ());
  { source = s.source
  ; close = s.close
  ; closed = s.closed
  ; node = s.node
  ; last = s.last }

let from_source source =
  let node = new_node () in
  let closed, close =
    Promise.create
      ()
  in
  {source; close; closed; node; last = ref node}

let from f =
  from_source
    (From {from_create = f; from_promise = Eio.Promise.create_resolved (Ok ())})

let from_direct f = from_source (From_direct f)
let closed s = Eio.Promise.await s.closed
let is_closed s = Eio.Promise.is_resolved s.closed

let enqueue' e last =
  let node = !last and new_last = new_node () in
  node.data <- e;
  node.next <- new_last;
  last := new_last

let enqueue e s = enqueue' e s.last

let create_with_reference () =
  (* Create the source for notifications of new elements. *)
  let source, push_signal_resolver =
    let push_signal, push_signal_resolver =
      Promise.create
        ()
    in
    ( {push_signal; push_waiting = false; push_external = Obj.repr ()}
    , ref push_signal_resolver )
  in
  let t = from_source (Push source) in
  (* [push] should not close over [t] so that it can be garbage collected even
   * there are still references to [push]. Unpack all the components of [t]
   * that [push] needs and reference those identifiers instead. *)
  let close = t.close and closed = t.closed and last = t.last in
  (* The push function. It does not keep a reference to the stream. *)
  let push x =
    if Eio.Promise.is_resolved closed then raise Closed;
    (* Push the element at the end of the queue. *)
    enqueue' x last;
    (* Send a signal if at least one thread is waiting for a new
       element. *)
    if source.push_waiting
    then (
      source.push_waiting <- false;
      (* Update threads. *)
      let old_push_signal_resolver = !push_signal_resolver in
      let new_waiter, new_push_signal_resolver =
        Promise.create
          ()
      in
      source.push_signal <- new_waiter;
      push_signal_resolver := new_push_signal_resolver;
      (* Signal that a new value has been received. *)
      ignore (Eio.Promise.try_resolve old_push_signal_resolver (Ok ())));
    (* Do this at the end in case one of the function raise an
       exception. *)
    if x = None then ignore (Eio.Promise.try_resolve close ())
  in
  t, push, fun x -> source.push_external <- Obj.repr x

let return a =
  let stream, push, _ = create_with_reference () in
  push (Some a); push None; stream

let return_promise a =
  let source, push, _ = create_with_reference () in
  Eliom_lib.fork (fun () ->
    try
      let x = Eio.Promise.await a in
      push (Some x); push None
    with _exc -> push None);
  source

let of_seq s =
  let s = ref s in
  let get () =
    match !s () with
    | Seq.Nil -> None
    | Seq.Cons (elt, s') ->
        s := s';
        Some elt
  in
  from_direct get

let create () =
  let source, push, _ = create_with_reference () in
  source, push

let of_iter iter i =
  let stream, push = create () in
  iter (fun x -> push (Some x)) i;
  push None;
  stream

let of_list l = of_iter List.iter l
let of_array a = of_iter Array.iter a
let of_string s = of_iter String.iter s

(* Add the pending element to the queue and notify the blocked pushed.

   Precondition: info.pushb_pending = Some _

   This does not modify info.pushb_count. *)
let notify_pusher info last =
  (* Push the element at the end of the queue. *)
  enqueue' info.pushb_pending last;
  (* Clear pending element. *)
  info.pushb_pending <- None;
  (* Wakeup the pusher. *)
  let old_wakener = info.pushb_push_wakener in
  let waiter, wakener =
    Promise.create
      ()
  in
  info.pushb_push_waiter <- waiter;
  info.pushb_push_wakener <- wakener;
  ignore (Eio.Promise.try_resolve old_wakener (Ok ()))

class ['a] bounded_push_impl (info : 'a push_bounded) wakener_cell last close =
  object
    val mutable closed = false
    method size = info.pushb_size

    method resize size =
      if size < 0 then invalid_arg "Eliom_stream.bounded_push#resize";
      info.pushb_size <- size;
      if info.pushb_count < info.pushb_size && info.pushb_pending <> None
      then (
        info.pushb_count <- info.pushb_count + 1;
        notify_pusher info last)

    method push x =
      if closed
      then raise Closed
      else if info.pushb_pending <> None
      then raise Full
      else if info.pushb_count >= info.pushb_size
      then (
        info.pushb_pending <- Some x;
        try Eio.Promise.await_exn info.pushb_push_waiter with
        | Cancelled ->
            info.pushb_pending <- None;
            let waiter, wakener = Eio.Promise.create () in
            info.pushb_push_waiter <- waiter;
            info.pushb_push_wakener <- wakener;
            raise Cancelled
        | exn -> raise exn)
      else (
        (* Push the element at the end of the queue. *)
        enqueue' (Some x) last;
        info.pushb_count <- info.pushb_count + 1;
        (* Send a signal if at least one thread is waiting for a new
         element. *)
        if info.pushb_waiting
        then (
          info.pushb_waiting <- false;
          (* Update threads. *)
          let old_wakener = !wakener_cell in
          let new_waiter, new_wakener =
            Promise.create
              ()
          in
          info.pushb_signal <- new_waiter;
          wakener_cell := new_wakener;
          (* Signal that a new value has been received. *)
          ignore (Eio.Promise.try_resolve old_wakener (Ok ()))))

    method close =
      if not closed
      then (
        closed <- true;
        let node = !last and new_last = new_node () in
        node.data <- None;
        node.next <- new_last;
        last := new_last;
        if info.pushb_pending <> None
        then (
          info.pushb_pending <- None;
          ignore (Eio.Promise.try_resolve info.pushb_push_wakener (Error Closed)));
        (* Send a signal if at least one thread is waiting for a new
         element. *)
        if info.pushb_waiting
        then (
          info.pushb_waiting <- false;
          let old_wakener = !wakener_cell in
          (* Signal that a new value has been received. *)
          ignore (Eio.Promise.try_resolve old_wakener (Ok ())));
        (* close is only resolved here and closed boolean prevents double call *)
        ignore (Eio.Promise.try_resolve close ()))

    method count = info.pushb_count
    method blocked = info.pushb_pending <> None
    method closed = closed

    method set_reference : 'a. 'a -> unit =
      fun x -> info.pushb_external <- Obj.repr x
  end

let create_bounded size =
  if size < 0 then invalid_arg "Eliom_stream.create_bounded";
  (* Create the source for notifications of new elements. *)
  let info, wakener_cell =
    let waiter, wakener =
      Promise.create
        ()
    in
    let push_waiter, push_wakener =
      Promise.create
        ()
    in
    ( { pushb_signal = waiter
      ; pushb_waiting = false
      ; pushb_size = size
      ; pushb_count = 0
      ; pushb_pending = None
      ; pushb_push_waiter = push_waiter
      ; pushb_push_wakener = push_wakener
      ; pushb_external = Obj.repr () }
    , ref wakener )
  in
  let t = from_source (Push_bounded info) in
  t, new bounded_push_impl info wakener_cell t.last t.close

(* Wait for a new element to be added to the queue of pending element
   of the stream. *)
let feed s =
  match s.source with
  | From from ->
      (* There is already a thread started to create a new element,
         wait for this one to terminate. *)
      if not (Eio.Promise.is_resolved from.from_promise)
      then from.from_promise
      else (* Otherwise request a new element. *)
        let promise, wakener = Eio.Promise.create () in
        Eliom_lib.fork (fun () ->
          try
            let x = from.from_create () in
            (* Push the element to the end of the queue. *)
            enqueue x s;
            if x = None then ignore (Eio.Promise.try_resolve s.close ());
            ignore (Eio.Promise.try_resolve wakener (Ok ()))
          with exn -> ignore (Eio.Promise.try_resolve wakener (Error exn)));
        (* Allow other threads to access this promise: *)
        from.from_promise <- promise;
        promise (*XXX protected *)
  | From_direct f ->
      let x = f () in
      (* Push the element to the end of the queue. *)
      enqueue x s;
      if x = None then ignore (Eio.Promise.try_resolve s.close ());
      Eio.Promise.create_resolved (Ok ())
  | Push push ->
      push.push_waiting <- true;
      push.push_signal (*XXX protected *)
  | Push_bounded push ->
      push.pushb_waiting <- true;
      push.pushb_signal (*XXX protected *)

(* Remove [node] from the top of the queue, or do nothing if it was
   already consumed.

   Precondition: node.data <> None
*)
let consume s node =
  if node == s.node
  then (
    s.node <- node.next;
    match s.source with
    | Push_bounded info ->
        if info.pushb_pending = None
        then info.pushb_count <- info.pushb_count - 1
        else notify_pusher info s.last
    | From _ | From_direct _ | Push _ -> ())

let rec peek_rec s node =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    peek_rec s node)
  else node.data

let peek s = peek_rec s s.node

let rec npeek_rec node acc n s =
  if n <= 0
  then List.rev acc
  else if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    npeek_rec node acc n s)
  else
    match node.data with
    | Some x -> npeek_rec node.next (x :: acc) (n - 1) s
    | None -> List.rev acc

let npeek n s = npeek_rec s.node [] n s

let rec get_rec s node =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    get_rec s node)
  else (
    if node.data <> None then consume s node;
    node.data)

let get s = get_rec s s.node

let rec get_exn_rec s node =
  if node == !(s.last)
  then
    match Eio.Promise.await_exn (feed s) with
    | () -> get_exn_rec s node
    | exception exn -> Some (Result.Error exn)
  else
    match node.data with
    | Some value -> consume s node; Some (Result.Ok value)
    | None -> None

let wrap_exn s = from (fun () -> get_exn_rec s s.node)

let rec nget_rec node acc n s =
  if n <= 0
  then List.rev acc
  else if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    nget_rec node acc n s)
  else
    match s.node.data with
    | Some x ->
        consume s node;
        nget_rec node.next (x :: acc) (n - 1) s
    | None -> List.rev acc

let nget n s = nget_rec s.node [] n s

let rec get_while_rec node acc f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    get_while_rec node acc f s)
  else
    match node.data with
    | Some x ->
        let test = f x in
        if test
        then (
          consume s node;
          get_while_rec node.next (x :: acc) f s)
        else List.rev acc
    | None -> List.rev acc

let get_while f s = get_while_rec s.node [] f s

let rec get_while_s_rec node acc f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    get_while_s_rec node acc f s)
  else
    match node.data with
    | Some x ->
        if f x
        then (
          consume s node;
          get_while_s_rec node.next (x :: acc) f s)
        else List.rev acc
    | None -> List.rev acc

let get_while_s f s = get_while_s_rec s.node [] f s

let rec next_rec s node =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    next_rec s node)
  else match node.data with Some x -> consume s node; x | None -> raise Empty

let next s = next_rec s s.node

let rec to_list_rec node acc s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    to_list_rec node acc s)
  else
    match node.data with
    | Some x ->
        consume s node;
        to_list_rec node.next (x :: acc) s
    | None -> List.rev acc

let to_list s = to_list_rec s.node [] s

let rec to_string_rec node buf s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    to_string_rec node buf s)
  else
    match node.data with
    | Some x ->
        consume s node;
        Buffer.add_char buf x;
        to_string_rec node.next buf s
    | None -> Buffer.contents buf

let to_string s = to_string_rec s.node (Buffer.create 128) s

let junk s =
  let node = s.node in
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    if node.data <> None then consume s node)
  else if node.data <> None
  then consume s node

let rec njunk_rec node n s =
  if n <= 0
  then ()
  else if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    njunk_rec node n s)
  else
    match node.data with
    | Some _ ->
        consume s node;
        njunk_rec node.next (n - 1) s
    | None -> ()

let njunk n s = njunk_rec s.node n s

let rec junk_while_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    junk_while_rec node f s)
  else
    match node.data with
    | Some x ->
        let test = f x in
        if test
        then (
          consume s node;
          junk_while_rec node.next f s)
        else ()
    | None -> ()

let junk_while f s = junk_while_rec s.node f s

let rec junk_while_s_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    junk_while_s_rec node f s)
  else
    match node.data with
    | Some x ->
        if f x
        then (
          consume s node;
          junk_while_s_rec node.next f s)
        else ()
    | None -> ()

let junk_while_s f s = junk_while_s_rec s.node f s

let rec junk_available_rec node s =
  if node == !(s.last)
  then
    match Eio.Promise.peek (feed s) with
    | Some (Ok _) -> junk_available_rec node s
    | Some (Error exn) -> raise exn
    | None -> ()
  else
    match node.data with
    | Some _ ->
        consume s node;
        junk_available_rec node.next s
    | None -> ()

let junk_available s = junk_available_rec s.node s

let rec get_available_rec node acc s =
  if node == !(s.last)
  then
    match Eio.Promise.peek (feed s) with
    | Some (Ok _) -> get_available_rec node acc s
    | Some (Error exn) -> raise exn
    | None -> List.rev acc
  else
    match node.data with
    | Some x ->
        consume s node;
        get_available_rec node.next (x :: acc) s
    | None -> List.rev acc

let get_available s = get_available_rec s.node [] s

let rec get_available_up_to_rec node acc n s =
  if n <= 0
  then List.rev acc
  else if node == !(s.last)
  then
    match Eio.Promise.peek (feed s) with
    | Some (Ok _) -> get_available_up_to_rec node acc n s
    | Some (Error exn) -> raise exn
    | None -> List.rev acc
  else
    match s.node.data with
    | Some x ->
        consume s node;
        get_available_up_to_rec node.next (x :: acc) (n - 1) s
    | None -> List.rev acc

let get_available_up_to n s = get_available_up_to_rec s.node [] n s

let rec is_empty s =
  if s.node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    is_empty s)
  else s.node.data = None

let map f s =
  from (fun () ->
    match get s with
    | Some x ->
        let x = f x in
        Some x
    | None -> None)

let map_s f s =
  from (fun () -> match get s with Some x -> Some (f x) | None -> None)

let filter f s =
  let rec next () =
    match get s with Some x as t -> if f x then t else next () | None -> None
  in
  from next

let filter_s f s =
  let rec next () =
    match get s with Some x as t -> if f x then t else next () | None -> None
  in
  from next

let filter_map f s =
  let rec next () =
    match get s with
    | Some x -> ( match f x with Some _ as t -> t | None -> next ())
    | None -> None
  in
  from next

let filter_map_s f s =
  let rec next () =
    match get s with
    | Some x -> ( match f x with None -> next () | t -> t)
    | None -> None
  in
  from next

let map_list f s =
  let pendings = ref [] in
  let rec next () =
    match !pendings with
    | [] -> (
      match get s with
      | Some x ->
          let l = f x in
          pendings := l;
          next ()
      | None -> None)
    | x :: l ->
        pendings := l;
        Some x
  in
  from next

let map_list_s f s =
  let pendings = ref [] in
  let rec next () =
    match !pendings with
    | [] -> (
      match get s with
      | Some x ->
          pendings := f x;
          next ()
      | None -> None)
    | x :: l ->
        pendings := l;
        Some x
  in
  from next

let flatten s = map_list (fun l -> l) s

let rec fold_rec node f s acc =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    fold_rec node f s acc)
  else
    match node.data with
    | Some x ->
        consume s node;
        let acc = f x acc in
        fold_rec node.next f s acc
    | None -> acc

let fold f s acc = fold_rec s.node f s acc

let rec fold_s_rec node f s acc =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    fold_s_rec node f s acc)
  else
    match node.data with
    | Some x ->
        consume s node;
        let acc = f x acc in
        fold_s_rec node.next f s acc
    | None -> acc

let fold_s f s acc = fold_s_rec s.node f s acc

let rec iter_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    iter_rec node f s)
  else
    match node.data with
    | Some x -> consume s node; f x; iter_rec node.next f s
    | None -> ()

let iter f s = iter_rec s.node f s

let rec iter_s_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    iter_s_rec node f s)
  else
    match node.data with
    | Some x -> consume s node; f x; iter_s_rec node.next f s
    | None -> ()

let iter_s f s = iter_s_rec s.node f s

let rec iter_p_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    iter_p_rec node f s)
  else
    match node.data with
    | Some x ->
        consume s node;
        Eio.Fiber.both (fun () -> f x) (fun () -> iter_p_rec node.next f s)
    | None -> ()

let iter_p f s = iter_p_rec s.node f s

let rec find_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    find_rec node f s)
  else
    match node.data with
    | Some x as opt ->
        consume s node;
        let test = f x in
        if test then opt else find_rec node.next f s
    | None -> None

let find f s = find_rec s.node f s

let rec find_s_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    find_s_rec node f s)
  else
    match node.data with
    | Some x as opt ->
        consume s node;
        if f x then opt else find_s_rec node.next f s
    | None -> None

let find_s f s = find_s_rec s.node f s

let rec find_map_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    find_map_rec node f s)
  else
    match node.data with
    | Some x ->
        consume s node;
        let x = f x in
        if x = None then find_map_rec node.next f s else x
    | None -> None

let find_map f s = find_map_rec s.node f s

let rec find_map_s_rec node f s =
  if node == !(s.last)
  then (
    Eio.Promise.await_exn (feed s);
    find_map_s_rec node f s)
  else
    match node.data with
    | Some x -> (
        consume s node;
        match f x with None -> find_map_s_rec node.next f s | t -> t)
    | None -> None

let find_map_s f s = find_map_s_rec s.node f s

let combine s1 s2 =
  let next () =
    let n1 = get s1 and n2 = get s2 in
    match n1, n2 with Some x1, Some x2 -> Some (x1, x2) | _ -> None
  in
  from next

let append s1 s2 =
  let current_s = ref s1 in
  let rec next () =
    match get !current_s with
    | Some _ as t -> t
    | None ->
        if !current_s == s2
        then None
        else (
          current_s := s2;
          next ())
  in
  from next

let concat s_top =
  let current_s = ref (from (fun () -> None)) in
  let rec next () =
    match get !current_s with
    | None -> (
      match get s_top with
      | Some s ->
          current_s := s;
          next ()
      | None -> None)
    | t -> t
  in
  from next

let parse s f =
  (match s.source with
  | Push_bounded _ -> invalid_arg "Eliom_stream.parse"
  | From _ | From_direct _ | Push _ -> ());
  let node = s.node in
  try f s
  with exn ->
    s.node <- node;
    raise exn

let hexdump stream =
  let buf = Buffer.create 80 and num = ref 0 in
  from (fun _ ->
    match nget 16 stream with
    | [] -> None
    | l ->
        Buffer.clear buf;
        Printf.bprintf buf "%08x|  " !num;
        num := !num + 16;
        let rec bytes pos = function
          | [] -> blanks pos
          | x :: l ->
              if pos = 8 then Buffer.add_char buf ' ';
              Printf.bprintf buf "%02x " (Char.code x);
              bytes (pos + 1) l
        and blanks pos =
          if pos < 16
          then (
            if pos = 8
            then Buffer.add_string buf "    "
            else Buffer.add_string buf "   ";
            blanks (pos + 1))
        in
        bytes 0 l;
        Buffer.add_string buf " |";
        List.iter
          (fun ch ->
             Buffer.add_char buf
               (if ch >= '\x20' && ch <= '\x7e' then ch else '.'))
          l;
        Buffer.add_char buf '|';
        Some (Buffer.contents buf))
