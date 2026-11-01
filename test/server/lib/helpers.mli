(** Helpers of the tests: requests checked with Alcotest, test cases. *)

val contains : string -> string -> bool
(** [contains s sub] tells whether [sub] occurs in [s]. *)

val check_response :
   ?status:int
  -> ?body:string
  -> string
  -> Browser.response
  -> unit
(** [check_response ?status ?body msg r] checks the status and the body of
    [r], when given. *)

val text : Browser.t -> string -> string Lwt.t
(** [text b url] is the body of the response to [url] for [b], checked to be a
    success. *)

val check : string -> string -> Browser.t -> string -> unit Lwt.t
(** [check msg expected b url] checks that [text b url] is [expected]. *)

val case :
   ?speed:Alcotest.speed_level
  -> Server_harness.t
  -> string
  -> (Browser.t -> unit Lwt.t)
  -> unit Alcotest.test_case
(** [case ?speed server name f] is a test that runs [f b] with a new browser
    [b], of speed [speed] ([`Quick] by default). *)

val case_browsers :
   Server_harness.t
  -> string
  -> ((unit -> Browser.t) -> unit Lwt.t)
  -> unit Alcotest.test_case
(** [case_browsers server name f] is a test that runs [f browser], where
    [browser ()] is a new browser. *)
