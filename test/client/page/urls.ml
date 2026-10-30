(* URLs computed in the same way by the server and by the client, which gets
   the services from the page: this module is compiled on both sides. *)

let uri = Eliom.Eliom_uri.make_string_uri

let cases (main, root, page, deep, suffix, non_attached, attached, external_) =
  [ "path", uri ~service:main (1, "x")
  ; "special characters", uri ~service:main (2, "é &=?/#+%")
  ; "absolute path", uri ~absolute_path:true ~service:main (3, "y")
  ; "absolute", uri ~absolute:true ~service:main (4, "z")
  ; "https", uri ~https:true ~service:main (5, "s")
  ; "fragment", uri ~fragment:"f r" ~service:main (6, "t")
  ; "root", uri ~service:root ()
  ; "same page", uri ~service:page ()
  ; "deeper", uri ~service:deep ()
  ; "suffix", uri ~service:suffix (7, "a/b")
  ; "non-attached coservice", uri ~service:non_attached ()
  ; "attached coservice", uri ~service:attached 8
  ; "external service", uri ~service:external_ "e"
  ; ( "preapplied"
    , uri ~service:(Eliom.Service.preapply ~service:main (1, "pre")) () ) ]
