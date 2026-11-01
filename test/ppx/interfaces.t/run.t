Interfaces. The server keeps the server and shared declarations, the client
keeps the client and shared declarations. Sections in interfaces take a
signature: `[%%client: ...]`.

  $ ppx_eliom_server --intf interfaces.eliomi
  val in_server : int
  val in_shared : int
  val let_server : int
  val let_shared : int
  val after_server_start : int

  $ ppx_eliom_client --intf interfaces.eliomi
  val in_client : int
  val in_shared : int
  val let_client : int
  val let_shared : int
  val after_client_start : int
