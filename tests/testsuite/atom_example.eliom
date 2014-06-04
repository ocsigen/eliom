(*
 * Copyright (C) 2010 Archibald Pontier
 *
 * This source file is part of Ocsigen < http://ocsigen.org/ >
 *
 * atom is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * atom is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with atom; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

module M = Html5.M
module Html5 = Eliom_content.Html5.F
open Atom_feed
open CalendarLib

let f () = Lwt.return (
      let r2 = Calendar.make 2009 11 22 13 54 21 in
      let d2 = Calendar.make 2010 7 1 18 12 1 in
      let d3 = Calendar.make 2012 12 11 16 14 36 in
      (* let's build the feed *)
      feed ~updated:r2 ~id:"http://test.org" ~title:(plain "Un flux Atom")
         (* the optional fields *)
         ~fields:[ authors [author "Tyruiop"]; subtitle (html5 [Html5.pcdata "Voilà un exemple du flux atom généré avec Ocsigen !"]);
               links [link ~elt:[`Rel ("alternate"); `Type ("text/html") ] "http://test.org"]]
         (* the entry list *)
         [entry ~updated:r2 ~id:"http://test.org/1" ~title:(plain "Article 1")
            (* the entry optional fields *)
            [authors [author ~elt:[uri "http://tyruiop.org"] "Tyruiop"];
            links [link "http://test.org/1"];
            summary (plain "Un petit résumé de l'article 1, avec un lien.");
            inlineC ["Un exemple de text content"]];
         entry ~updated:d2 ~id:"http://test.org/2" ~title:(plain "Article 2")
            [authors [author "Tyruiop"];
            summary (plain "Un petit résumé de l'article 2");
            published d2;
            html5C [Html5.pcdata "un exemple de content en xHTML !"]];
         entry ~updated:d3 ~id:"http://test.org/3" ~title:(plain "Article 3")
            [authors [author "bépo"];
             summary
               (html5
                  [Html5.pcdata "Un petit résumé de l'article 3 en ";
                   Html5.a
                     ~service:Eliom_testsuite_base.main
                     [Html5.pcdata "Html5"]
                     ();
                  ]
               );
            published d3;
            html5C [Html5.pcdata "un exemple de content en HTML5 !"]];
   ]
   )

(*(* let's register the feed *)
let s = Eliom_atom.Reg.register_new_service ~path:["test"] ~get_params:Eliom_parameter.unit f
*)
let s = Eliom_atom.register_feed ~path:["test"] ~hubs:["http://tyruiop.org:8888"; "http://pubsubhubbub.appspot.com"] "http://tyruiop.org:8080/test/test" f
