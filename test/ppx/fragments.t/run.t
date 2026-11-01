Client values. The server creates a reference to a closure registered by the
client, with the values sent from the server. The type pass binds each client
value and each sent value to a reference whose type is inferred on the server.
Without inferred types, the client uses `_`. The outputs are put on one line,
to check parts of them independently of the layout.

  $ flat () { tr -s ' \n' '  '; }
  $ ppx_eliom_server --impl fragments.eliom > server.ml
  $ ppx_eliom_types --impl fragments.eliom > types.ml
  $ ppx_eliom_client --impl fragments.eliom > client.ml

The server sends the identifier of each client value, with the values used in
it. The same value used twice is sent once. The type of a client value comes
from its annotation:

  $ flat < server.ml | grep -Eo '"16vYpk[0-9]+" [^:]* : [^)]*Client_value\.t' |
  >   sed 's/Eliom\.Syntax\.//g'
  "16vYpk1" () : _ Eliom.Client_value.t
  "16vYpk2" () : unit Eliom.Client_value.t
  "16vYpk3" ((escaped_value (_eliom_escaped_ident_1 x)), (escaped_value (_eliom_escaped_ident_2 y))) : _ Eliom.Client_value.t
  "16vYpk4" (escaped_value (_eliom_escaped_ident_3 (x + 1))) : _ Eliom.Client_value.t
  "16vYpk5" (escaped_value (_eliom_escaped_ident_4 x)) : _ Eliom.Client_value.t
  "16vYpk6" () : _ Eliom.Client_value.t
  "16vYpk7" (escaped_value (_eliom_escaped_ident_5 x)) : _ Eliom.Client_value.t
  "16vYpk8" () : _ Eliom.Client_value.t

Each client value goes through a function that stores it in a reference, which
gives it a monomorphic type. The unsafe client value (6) does not, so it may be
polymorphic. A shared value is also computed on the server:

  $ grep -Eo 'let _eliom_fragment_[^ ]*( y)? =|Eliom\.Shared\.Value\.create .*' server.ml
  let _eliom_fragment_16vYpk1 =
  let _eliom_fragment_16vYpk2 =
  let _eliom_fragment_16vYpk3 =
  let _eliom_fragment_16vYpk4 =
  let _eliom_fragment_16vYpk5 =
  let _eliom_fragment_16vYpk6 y =
  let _eliom_fragment_16vYpk7 =
  Eliom.Shared.Value.create (x + 1)
  let _eliom_fragment_16vYpk8 =

The type pass stores the client values and the sent values in references:

  $ flat < types.ml |
  >   grep -Eo '_eliom_[a-z_]+[0-9A-Za-z]* := \(Some ([^()]|\([^()]*\))*\)'
  _eliom_fragment_16vYpk1 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))
  _eliom_fragment_16vYpk2 := (Some (Eliom.Syntax.client_value "" 0 : unit Eliom.Client_value.t))
  _eliom_escaped_ident_1 := (Some x)
  _eliom_escaped_ident_2 := (Some y)
  _eliom_fragment_16vYpk3 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))
  _eliom_escaped_ident_3 := (Some (x + 1))
  _eliom_fragment_16vYpk4 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))
  _eliom_escaped_ident_4 := (Some x)
  _eliom_fragment_16vYpk5 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))
  _eliom_fragment_16vYpk6 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))
  _eliom_escaped_ident_5 := (Some x)
  _eliom_fragment_16vYpk7 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))
  _eliom_fragment_16vYpk8 := (Some (Eliom.Syntax.client_value "" 0 : _ Eliom.Client_value.t))

The client registers a closure for each client value, taking the sent values.
A value used twice is read twice:

  $ flat < client.ml |
  >   grep -Eo 'register_client_closure "[^"]*" \(fun (\(\)|[a-z_0-9]+|\([^)]*\))|get_escaped_value _eliom_escaped_ident_[0-9]+'
  register_client_closure "16vYpk1" (fun ()
  register_client_closure "16vYpk2" (fun ()
  register_client_closure "16vYpk3" (fun (_eliom_escaped_ident_1, _eliom_escaped_ident_2)
  get_escaped_value _eliom_escaped_ident_1
  get_escaped_value _eliom_escaped_ident_2
  get_escaped_value _eliom_escaped_ident_1
  register_client_closure "16vYpk4" (fun _eliom_escaped_ident_3
  get_escaped_value _eliom_escaped_ident_3
  register_client_closure "16vYpk5" (fun _eliom_escaped_ident_4
  get_escaped_value _eliom_escaped_ident_4
  register_client_closure "16vYpk6" (fun ()
  register_client_closure "16vYpk7" (fun _eliom_escaped_ident_5
  get_escaped_value _eliom_escaped_ident_5
  register_client_closure "16vYpk8" (fun ()

In shared code, the client side calls the function of the client value
directly:

  $ flat < client.ml | grep -Eo 'let from_shared x = [^;]*[^ ;]'
  let from_shared x = let _eliom_escaped_ident_4 = x in _eliom_fragment_16vYpk5 _eliom_escaped_ident_4
