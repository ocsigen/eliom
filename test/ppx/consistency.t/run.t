The client must read back the data the server sends, in the same order. The
server and the client outputs of the same file agree.

  $ ppx_eliom_server --impl app.eliom > server.ml
  $ ppx_eliom_client --impl app.eliom > client.ml

The server sends the client values of each server section and the injections of
each client section; the client reads them in the same order:

  $ sections () { grep -Eo "$1|$2" | sed -e "s/$1/server/" -e "s/$2/client/"; }
  $ sections close_server_section close_client_section < server.ml > server
  $ sections close_server_section open_client_section < client.ml > client
  $ diff server client
  $ cat client
  server
  client
  client
  server
  server
  client

The client registers a closure for each client value created by the server:

  $ grep -o '}) "[^"]*"' server.ml | cut -d '"' -f 2 | sort > server
  $ grep -o 'register_client_closure "[^"]*"' client.ml | cut -d '"' -f 2 | sort > client
  $ diff server client
  $ cat client
  qWHe1U1
  qWHe1U2
  qWHe1U3
  qWHe1U4
  qWHe1U5
  qWHe1U6

The client reads each injection the server sends:

  $ sed -En 's/^(let|and) _eliom_injected_ident_([^ ]*) = .+/\2/p' server.ml | sort > server
  $ grep -o '}) "[^"]*"' client.ml | cut -d '"' -f 2 | sort -u > client
  $ diff server client
  $ cat client
  qWHe1U1
  qWHe1U2
  qWHe1U3
