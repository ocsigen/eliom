val in_server : int

[%%client:
val in_client : int]

[%%shared:
val in_shared : int]

val%server let_server : int
val%client let_client : int
val%shared let_shared : int

[%%client.start]

val after_client_start : int

[%%server.start]

val after_server_start : int
