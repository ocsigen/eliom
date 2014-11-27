Eliom

------------------------------------------------------------------

Requirements:
=============

Compilers:

 * ocaml and camlp4  (need version >= 4.0)
 * js_of_ocaml       (need version >= 2.2, with deriving support)
 * a C compiler      (tested with gcc-4.4.5)

Libraries:

 * findlib
 * ocsigenserver     (need version >= 2.4)
 * tyxml             (need version >= 3)
 * react             (need version >= 0.9.3)
 * deriving          (need version >= 0.6)
                     See: https://github.com/ocsigen/deriving
 * ocamlssl          (tested with 0.4.6)
 * calendar          (tested with 2.03.1)
 * optcomp
 * ipaddr            (need version >= 2.1)



------------------------------------------------------------------

Build instructions:
===================

We recommand to use the opam package manager to install Eliom.
If you want to compile manually:

 * run "make" to compile
 * run "make PREFIX=yourprefix install" as root to install
 * run "make PREFIX=yourprefix uninstall" to uninstall everything

------------------------------------------------------------------

Local testings:
===============
 * install eliom

 * run "make run.local" of "make run.opt.local"
   in the ocsigen source directory.

 * open http://localhost:8080/miniwiki in your browser

 * if it does not work, look at the logs (see 'local/var/log/' in the
   ocsgigen source directory) or run ocsigen with options -v or -V
   (verbose and debug mode).

 * sources for this example may be found in the directory
   'tests/miniwiki'. For a full tutorial, see:

      http://ocsigen.org/tutorial

    For testsuite, see:

      http://localhost:8080/

------------------------------------------------------------------

Authors:
========

Vincent Balat
J�r�me Vouillon
Gr�goire Henry
Pierre Chambart
Benedikt Becker
Boris Yakobowski
Hugo Heuzard
Rapha�l Proust
St�phane Glondu
Gabriel Kerneis
Denis Berthod
Jaap Boender
Simon Castellan
Mauricio Fernandez
Archibald Pontier
Simon Castellan
Jacques-Pascal Deplaix
