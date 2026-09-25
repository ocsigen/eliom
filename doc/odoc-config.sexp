; Per-package odoc configuration (read by odoc_driver, both on ocaml.org and
; by wodoc). Declares the dependencies whose modules and pages the manual
; references with the {!/package-or-library/...} path syntax.
(libraries
 ocsigenserver
 ocsigenserver.baselib
 lwt.unix
 ocsipersist
 ocsigen-start.server
 ocsigen-toolkit.client)
(packages ocsigenserver ocsigen-start ocsigen-toolkit tyxml lwt)
