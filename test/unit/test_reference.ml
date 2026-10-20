(* [Eliom.Reference.open_on_first_use] opens the store of a global persistent
   reference at its first use, not when the reference is created: concurrent
   callers share one opening, a success is kept, a failure is tried again. *)

let open_on_first_use = Eliom.Reference.open_on_first_use

type 'a state = Pending | Fulfilled of 'a | Rejected of exn

let state p =
  match Lwt.state p with
  | Lwt.Sleep -> Pending
  | Lwt.Return v -> Fulfilled v
  | Lwt.Fail e -> Rejected e

let pp_state fmt = function
  | Pending -> Format.pp_print_string fmt "pending"
  | Fulfilled v -> Format.fprintf fmt "fulfilled with %d" v
  | Rejected e -> Format.fprintf fmt "rejected with %s" (Printexc.to_string e)

let check msg expected p =
  Alcotest.check (Alcotest.testable pp_state ( = )) msg expected (state p)

(* [opening ()] is an opening whose attempts are counted in [attempts] and
   resolved by the test, through the resolvers pushed in [pending]. *)
let opening () =
  let attempts = ref 0 and pending = Queue.create () in
  let open_ =
    open_on_first_use (fun () ->
      incr attempts;
      let p, u = Lwt.task () in
      Queue.push u pending; p)
  in
  open_, attempts, pending

let not_configured = Failure "not configured"

let test_retry () =
  let open_, attempts, pending = opening () in
  Alcotest.(check int) "nothing opened at creation" 0 !attempts;
  let p1 = open_ () in
  let p2 = open_ () in
  Alcotest.(check int) "one opening for concurrent callers" 1 !attempts;
  check "shared opening pending" Pending p2;
  Lwt.wakeup_exn (Queue.pop pending) not_configured;
  check "first caller gets the failure" (Rejected not_configured) p1;
  check "second caller too" (Rejected not_configured) p2;
  let p3 = open_ () in
  Alcotest.(check int) "opened again after a failure" 2 !attempts;
  check "new opening pending" Pending p3;
  Lwt.wakeup (Queue.pop pending) 42;
  check "fulfilled" (Fulfilled 42) p3;
  check "success shared" (Fulfilled 42) (open_ ());
  Alcotest.(check int) "success kept" 2 !attempts

let test_exception () =
  let attempts = ref 0 in
  let open_ =
    open_on_first_use (fun () ->
      incr attempts;
      if !attempts = 1 then raise not_configured else Lwt.return 1)
  in
  check "exception as a rejection" (Rejected not_configured) (open_ ());
  check "opened again" (Fulfilled 1) (open_ ())

let test_cancel () =
  let open_, _, pending = opening () in
  let p1 = open_ () in
  let p2 = open_ () in
  Lwt.cancel p1;
  check "cancelled caller" (Rejected Lwt.Canceled) p1;
  check "shared opening not cancelled" Pending p2;
  Lwt.wakeup (Queue.pop pending) 7;
  check "other caller fulfilled" (Fulfilled 7) p2

(* Lwt may run the failure handler of a caller before the other callbacks of
   the rejected promise: a retry from that handler must not get the
   rejection. *)
let test_retry_from_handler () =
  let open_, attempts, pending = opening () in
  let p = Lwt.catch open_ (fun _ -> open_ ()) in
  Lwt.wakeup_exn (Queue.pop pending) not_configured;
  Alcotest.(check int) "opened again from the handler" 2 !attempts;
  check "new opening pending" Pending p;
  Lwt.wakeup (Queue.pop pending) 3;
  check "fulfilled" (Fulfilled 3) p

let suite =
  ( "reference"
  , [ Alcotest.test_case "retry after a failure" `Quick test_retry
    ; Alcotest.test_case "exception" `Quick test_exception
    ; Alcotest.test_case "cancel" `Quick test_cancel
    ; Alcotest.test_case "retry from a failure handler" `Quick
        test_retry_from_handler ] )
