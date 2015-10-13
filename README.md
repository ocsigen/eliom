# Eliom

Eliom is a framework for building web sites and client/server web
applications in OCaml. Eliom employs sophisticated techniques from the
field of functional programming, thus enabling the programmer to build
complex web sites in very few lines of code. Both the server and the
client parts of an application are implemented in OCaml, as a single
program.

Eliom is part of the [Ocsigen project][ocsigen].

## Installation Instructions

We recommend that you use the [OPAM package manager][opam] to install
Eliom. Once you [have installed OPAM][opaminst], all you have to do is
type:

    opam install eliom

If you want to compile manually,

 * run `make` to compile;
 * run `make PREFIX=${YOURPREFIX} install` (as root if necessary) to
   install; and
 * run `make PREFIX=${YOURPREFIX} uninstall` to uninstall everything.

## Testing locally

* Install Eliom.

* Run `make run.local` or `make run.opt.local` in the Eliom source
  directory.

* Open <http://localhost:8080/miniwiki> in your browser.

* If the above does not work, look at the logs (see `local/var/log/`
  in the Eliom source directory) or run Eliom with either of the
  options `-v`, `-V` (verbose and debug mode, respectively).

* Sources for this example may be found in the directory
  `tests/miniwiki`. A complete tutorial is
  [available online][tutorial]. For a testsuite, see
  <http://localhost:8080/>.

## Authors

* Vincent Balat
* Jérôme Vouillon
* Grégoire Henry
* Pierre Chambart
* Benedikt Becker
* Boris Yakobowski
* Hugo Heuzard
* Raphaël Proust
* Stéphane Glondu
* Gabriel Kerneis
* Denis Berthod
* Jaap Boender
* Simon Castellan
* Mauricio Fernandez
* Archibald Pontier
* Simon Castellan
* Jacques-Pascal Deplaix

[ocsigen]: https://www.ocsigen.org/
[tutorial]: https://ocsigen.org/tuto/manual/
[opam]: https://opam.ocaml.org/
[opaminst]: https://opam.ocaml.org/doc/Install.html