(* $Id: www_ls.ml,v 1.3 2004/01/25 23:45:03 ohl Exp $ *)

open XHTML.M

let item name =
  li [a ~a:[a_href name] [code [pcdata name]]]

let ls url files =
  let heading =
    match url with
    | None -> "Directory listing"
    | Some u -> "Directory listing of " ^ u in
  html ~a:[a_xmlns `W3_org_1999_xhtml; a_xml_lang "en"]
    (head (title (pcdata heading)) [])
    (body
       [h1 [pcdata heading];
        p [pcdata "toto"; em [pcdata "titi"]; pcdata "toto"];
        match files with
        | [] -> p [pcdata "empty!"]
        | first :: rest -> ul (item first) (List.map item rest)])

let usage = "usage: ./www_ls [-url url] [files]"

let _ =
  let url = ref None
  and files = ref [] in
  Arg.parse
    [ ("-url", Arg.String (fun s -> url := Some s), "set url") ]
    (fun n -> files := n :: !files)
    usage;
  pretty_print ~width:2 print_string (ls !url !files)
