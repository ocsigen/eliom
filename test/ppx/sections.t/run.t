Toplevel code goes to the server by default. Each pass keeps the code of its
sides: the server keeps the server and shared code, the client keeps the client
and shared code, and the type pass, which infers the types of what the server
sends to the client, keeps the server and shared code. The values defined in
each output:

  $ defined () { grep -Eo 'let [a-z][a-z_]* =' | cut -d ' ' -f 2; }
  $ ppx_eliom_server --impl sections.eliom | defined
  default
  in_shared
  in_server
  let_shared
  let_server
  after_shared_start
  after_server_start
  included

  $ ppx_eliom_types --impl sections.eliom | defined
  default
  in_shared
  in_server
  let_shared
  let_server
  after_shared_start
  after_server_start
  included

  $ ppx_eliom_client --impl sections.eliom | defined
  in_client
  in_shared
  let_client
  let_shared
  after_client_start
  after_shared_start
  included
  in_module

To compile Eliom itself, `-internal` refers to its modules without the `Eliom`
prefix:

  $ ppx_eliom_server -internal --impl internal.eliom |
  >   grep -Eo '(Eliom\.)?Syntax\.[a-z_]+' | sort -u
  Syntax.close_client_section
  Syntax.set_global
  Syntax.to_poly

  $ ppx_eliom_client -internal --impl internal.eliom |
  >   grep -Eo '(Eliom\.)?Client_core\.[A-Za-z_.]+' | sort -u
  Client_core.Syntax_helpers.get_injection
  Client_core.Syntax_helpers.open_client_section
