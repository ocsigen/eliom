(* $Id: homepage.ml,v 1.6 2005/06/20 17:57:58 ohl Exp $ *)

let self = "examples/homepage.ml"

let file_to_string name =
  let ic = open_in name in
  let len = in_channel_length ic in
  let s = String.create len in
  really_input ic s 0 len;
  close_in ic;
  s

let paragraph_separator = Str.regexp "\n\n+"

let file_to_paragraphs name =
  Str.split paragraph_separator (file_to_string name)

open XHTML.M

let href url elts =
  a ~a:[a_href url] elts

let file name =
  href name [code [pcdata name]]

let valid_xhtml ?url ~name ~email () =
  table ~a:[a_width (`Percent 100)]
    (tr
       (td [pcdata "This WWW page is brought to you by ";
	    (match url with None -> pcdata name | Some u -> href u [pcdata name]);
	    pcdata " "; href ("email:" ^ email) [pcdata ("<" ^ email ^ ">")];
	    pcdata ".  It is valid "; href standard [pcdata version];
	    pcdata ", as can be verified online by going to the ";
	    href "http://www.w3.org/" [pcdata "W3C"]; pcdata " ";
	    href validator [pcdata "MarkUp Validation Service"]; pcdata "."])
       [td [validator_icon ()]])
    []

let sample tar_gz =
  html ~a:[a_xmlns `W3_org_1999_xhtml; a_xml_lang "en"]
    (head
       (title (pcdata "XHTML"))
       [style ~contenttype:"text/css" [pcdata "H1 {color: red}"]])
    (body
       ([h1 [pcdata "A sample page for the XHTML module"];
	 h2 [pcdata "Readme"];
	 p [pcdata "The following is imported "; em [pcdata "verbatim"];
	    pcdata " from the "; href "README" [pcdata "README"];
	    pcdata " file, as ca be seen in the ";
	    href "#source" [pcdata "source code of this page"];
	    pcdata " below."]] @
	List.map (fun s -> p [pcdata s]) (file_to_paragraphs "README") @
	[h2 [pcdata "Example"];
	 p [pcdata "This is a recursive example that contains its own description ";
	    entity "hellip"];
	 h2 [a ~a:[a_id "source"] [pcdata "Code"]];
	 pre [pcdata (file_to_string self)];
	 h2 [pcdata "Sources"];
	 p [pcdata "The current sources are available as a gzip compressed tar archive: ";
	    file tar_gz; pcdata ".  It includes";
	    entity "mdash"; pcdata "among others"; entity "mdash"; pcdata "the files"];
	 dl
	   (dt [file self])
	   [dd [pcdata "this example"];
	    dt [file "xML.mli"];
	    dd [pcdata "the interface XML, providing well formed documents"];
	    dt [file "xML.ml"];
	    dd [pcdata "the implementation of the XML interface"];
	    dt [file "xHTML.mli"];
	    dd [pcdata "the interface XHTML, providing documents, that are ";
		pcdata "valid according to the XHTML 1.1 DTD."];
	    dt [file "xHTML.ml"];
	    dd [pcdata "the implementation of the interface XHTML"];
	    dt [file "ChangeLog"];
	    dd [pcdata "history of changes"]];
	 hr ();
	 valid_xhtml
	   ~name:"Thorsten Ohl"
	   ~url:"http://theorie.physik.uni-wuerzburg.de/~ohl/"
	   ~email:"ohl@physik.uni-wuerzburg.de"
	   ()]))

let _ =
  let tar_gz = ref "xhtml-current.tar.gz" in
  Arg.parse [] (fun s -> tar_gz := s) "usage: homepage [archive]";
  pretty_print ~width:72 print_string (sample !tar_gz)
