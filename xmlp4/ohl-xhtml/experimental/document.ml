(* $Id: document.ml,v 1.2 2004/01/26 13:29:06 ohl Exp $ *)

module type T =
  sig
    module X : XHTML.T
    type t
    val empty : string -> t
    val append : string ->
      [ X.heading | X.block | X.LIST.list ] X.elt list -> t -> t
    val add_style_internal : ?title:string -> string list -> t -> t
    val add_style_external : X.uri -> t -> t
    val to_file : ?multi:string -> string -> t -> unit
    val to_files : ?single:string -> string -> t -> unit
  end


let rec concat_elts joint = function
  | [] -> []
  | [head] -> [head]
  | head :: tail -> head :: joint :: concat_elts joint tail
				      
let rec concat_lists joint = function
  | [] -> []
  | [head] -> head
  | head :: tail -> head @ joint @ concat_lists joint tail
				     
(*

let fold2_rev f l1 l2 acc =
  List.fold_left (fun acc1 x1 ->
    List.fold_left (fun acc2 x2 -> f x1 x2 acc2) acc1 l2) acc l1

let fold2 f l1 l2 acc =
  fold2_rev f (List.rev l1) (List.rev l2) acc

 *)

module type URL =
  sig
    type t
    val create : ?base:string -> ?name:string -> ?anchor:string -> unit -> t
    val to_string : t -> string
    val file_name : t -> string
    val append_to_name : string -> t -> t
    val add_anchor : string -> t -> t
  end

module URL : URL =
  struct

    type t =
	{ base : string option;
	  name : string option;
	  anchor : string option }

    let create ?base ?name ?anchor () =
      { base = base;
	name = name;
	anchor = anchor }

    let to_string url =
      (match url.base with None -> "" | Some b -> b ^ "/") ^
      (match url.name with None -> "" | Some n -> n ^ ".html") ^
      (match url.anchor with None -> "" | Some i -> "#" ^ i)

    let file_name url =
      (match url.base with None -> "" | Some b -> b ^ "/") ^
      (match url.name with None -> "" | Some n -> n ^ ".html")

    let append_to_name suffix url =
      let name =
	(match url.name with
	 | None -> ""
	 | Some n -> n) ^ "_" ^ suffix in
      { url with name = Some name }

    let add_anchor anchor url =
      { url with anchor = Some anchor }

  end

module Make (X : XHTML.T) : T with module X = X =
  struct

    module X = X
    open X

    let href url elts =
      a ~a:[a_href (URL.to_string url)] elts

    let href_email adr =
      a ~a:[a_href ("email:" ^ adr)] [pcdata ("<" ^ adr ^ ">")]
        
    let href_person ?url name =
      match url with
      | Some url -> a ~a:[a_href url] [pcdata name]
      | None -> pcdata name

    type section =
	{ (* A human readable description, used for the link text.  *)
	  label : string;
          
          (* A unique identifier, must be suitable as part of a file name and
	     as a in-page link. *)
          anchor : string;
	  
          (* The content proper. *)
	  content : [ heading | block | LIST.list ] elt list }

    type style =
      | External of uri
      | Internal of string option * string list

    type t =
	{ title : string;
          rev_sections : section list;
          style : style list }
          
    let empty title =
      { title = title;
        rev_sections = [];
        style = [] }

    module CSet = Set.Make (struct type t = char let compare = compare end)
    let unsafe_chars = List.fold_right CSet.add [' '; '/'] CSet.empty
    let is_unsafe c = CSet.mem c unsafe_chars

    let anchor_of_label label =
      let anchor = String.lowercase label in
      for i = 0 to String.length anchor - 1 do
        if is_unsafe anchor.[i] then
          anchor.[i] <- '_'
      done;
      anchor

    let append label content d =
      { d with rev_sections = { label = label;
				anchor = anchor_of_label label;
				content = content } :: d.rev_sections }

    let add_style_internal ?title css d =
      { d with style = Internal (title, css) :: d.style }
        
    let add_style_external uri d =
      { d with style = External uri :: d.style }
        
    let style_elt = function
      | External name ->
          link ~a:[a_href name; a_rel [`Stylesheet]; a_type "text/css"] ()
      | Internal (None, css) ->
          style ~contenttype:"text/css"
            (List.map (fun s -> pcdata (s ^ " ")) css)
      | Internal (Some title, css) ->
          style ~contenttype:"text/css" ~a:[a_title title]
            (List.map (fun s -> pcdata (s ^ " ")) css)


    module Id_Set = Set.Make (struct type t = id let compare = compare end)
    let id_set_of_list ids =
      List.fold_right Id_Set.add ids Id_Set.empty
          
    type page =
	{ section : section;
          file_name : URL.t option;
	  anchors : Id_Set.t }

    module Href_Map = Map.Make (struct type t = id let compare = compare end)

    let add_to_href_map name anchor map =
      let from_url = URL.create ~anchor ()
      and to_url = URL.add_anchor anchor name in
      (* Printf.eprintf "%s -> %s\n" (URL.to_string from_url) (URL.to_string to_url); *)
      Href_Map.add (URL.to_string from_url) to_url  map
	  
    let grow_href_map to_page map =
      match to_page.file_name with
      | None -> map
      | Some name -> Id_Set.fold (add_to_href_map name) to_page.anchors map

    let href_map pages =
      List.fold_right grow_href_map pages Href_Map.empty
      

    type rel_link =
      | Active_Relative of page
      | Inactive_Relative

    type abs_link =
      | Active of page
      | Inactive of page

    let url_of_page page =
      match page.file_name with
      | None -> URL.create ~anchor:page.section.anchor ()
      | Some name -> URL.add_anchor page.section.anchor name

    let elt_of_rel_link label = function
      | Active_Relative page -> href (url_of_page page) [pcdata label]
      | Inactive_Relative -> pcdata label

    let link_of_rel_link rel = function
      | Active_Relative page ->
	  [link ~a:[a_href (URL.to_string (url_of_page page));
		    a_rel [rel]] ()]
      | Inactive_Relative -> []

    let elt_of_abs_link = function
      | Active page -> href (url_of_page page) [pcdata page.section.label]
      | Inactive page -> pcdata page.section.label

    type xref =
	{ first : rel_link;
	  prev : rel_link;
	  next : rel_link;
	  last : rel_link;
	  sections : abs_link list;
	  self : page }

    let xref section backward self forward =
      let rel_link_of_list = function
	| [] -> Inactive_Relative
	| head :: _ -> Active_Relative head in
      { first = rel_link_of_list (List.rev backward);
	prev = rel_link_of_list backward;
	next = rel_link_of_list forward;
	last = rel_link_of_list (List.rev forward);
	sections =
	List.rev_map (fun p -> Active p) backward @
	[Inactive self] @
	List.map (fun p -> Active p) forward;
	self = self }

    let rec xrefs_of_sections' links = function
      | [] -> invalid_arg "xrefs_of_sections' _ []"
      | [section] ->
          [xref section (Zipper.rev_left links) (Zipper.center links) []]
      | section1 :: sections ->
	  xref section1 (Zipper.rev_left links) (Zipper.center links) (Zipper.right links) ::
          xrefs_of_sections' (Zipper.step_right links) sections

    let xrefs_of_sections mk_link = function
      | [] -> []
      | [section] ->
          [xref section [] (mk_link section) []]
      | section1 :: sections2 as sections ->
          let links = Zipper.of_list (List.map mk_link sections) in
          xref section1 [] (Zipper.center links) (Zipper.right links) ::
          xrefs_of_sections' (Zipper.step_right links) sections2

    let p_of_xref ?single ?multi x =
      p ~a:[a_class ["navigation"]]
	([pcdata "Navigation: ";
	  elt_of_rel_link "First" x.first;
	  pcdata ", ";
	  elt_of_rel_link "Previous" x.prev;
	  pcdata ", ";
	  elt_of_rel_link "Next" x.next;
	  pcdata ", ";
	  elt_of_rel_link "Last" x.last;
	  pcdata ". Pages: "] @
	 concat_elts (pcdata ", ") (List.map elt_of_abs_link x.sections) @
	 [pcdata ". "] @
	 (match single with
	 | None -> []
	 | Some s -> [href s [pcdata "(single file version)"]]) @
	 (match multi with
	 | None -> []
	 | Some m -> [href m [pcdata "(multi file version)"]]))

    let table_of_xref ?single ?multi x =
      table ~a:[a_class ["navigation"]; a_width (`Percent 100)]
	(tr ~a:[a_class ["absolute"]]
	   (td ~a:[a_colspan 5; a_align `Center]
	      ((concat_elts (pcdata ", ") (List.map elt_of_abs_link x.sections)) @
	       [pcdata ". "]))
	   [])
	[tr ~a:[a_class ["relative"]]
	   (td ~a:[a_align `Left] [elt_of_rel_link "First" x.first])
	   [td ~a:[a_align `Left] [elt_of_rel_link "Previous" x.prev];
	    td ~a:[a_align `Center]
	      ([space ()] @
	       (match single with
	        | None -> []
	        | Some s -> [href s [pcdata "(single file version)"]]) @
	       (match multi with
	        | None -> []
		| Some m -> [href m [pcdata "(multi file version)"]]) @
	       [space ()]);
	    td ~a:[a_align `Right] [elt_of_rel_link "Next" x.next];
	    td ~a:[a_align `Right] [elt_of_rel_link "Last" x.last]]]

    let ul_of_xref ?single ?multi x =
      ul ~a:[a_class ["navigation"]]
	(li
	   ([pcdata "Navigation: ";
	     elt_of_rel_link "First" x.first;
	     pcdata ", ";
	     elt_of_rel_link "Previous" x.prev;
	     pcdata ", ";
	     elt_of_rel_link "Next" x.next;
	     pcdata ", ";
	     elt_of_rel_link "Last" x.last;
	     pcdata ". "] @
	    (match single with
	    | None -> []
	    | Some s -> [href s [pcdata "(single file version)"]]) @
	    (match multi with
	    | None -> []
	    | Some m -> [href m [pcdata "(multi file version)"]])))
	[li
	   ([pcdata "Pages: "] @
	    concat_elts (pcdata ", ") (List.map elt_of_abs_link x.sections) @
	    [pcdata ". "])]

    let format_xref = ul_of_xref
    let format_xref = table_of_xref

    let valid_xhtml ?url ~name ~email () =
      table ~a:[a_width (`Percent 100)]
	(tr
	   (td [pcdata "This WWW page is brought to you by ";
		href_person ?url name;
		pcdata " ";
		href_email email;
		pcdata ".  It is valid ";
		a ~a:[a_href standard] [pcdata version];
		pcdata ", as can be verified online by going to the ";
		a ~a:[a_href "http://www.w3.org/"] [pcdata "W3C"];
		pcdata " ";
		a ~a:[a_href validator] [pcdata "MarkUp Validation Service"];
		pcdata "."])
	   [td [validator_icon ()]])
	[]

    let address =
      [valid_xhtml ~url:"http://theorie.physik.uni-wuerzburg.de/~ohl/"
	 ~name:"Thorsten Ohl" ~email:"ohl@physik.uni-wuerzburg.de" ()]

    let page_to_file name page =
      let oc = open_out name in
      pretty_print ~width:72 ~encode:XML.encode_unsafe_and_at (output_string oc) page;
      close_out oc

    let body_to_file name ~title:t
	?style:(s = []) ?links:(l = []) body_elts =
      let page =
        html ~a:[a_xmlns `W3_org_1999_xhtml; a_xml_lang "en"]
          (head (title (pcdata t)) (l @ List.map style_elt s))
          (body (body_elts @ (hr () :: address))) in
      page_to_file name page

    let single_page section =
      { section = section;
        file_name = None;
	anchors = id_set_of_list (List.flatten (List.map all_anchors section.content)) }

    let multi_page prefix section =
      let name = URL.create ~name:(prefix ^ "_" ^ section.anchor) () in
      { section = section;
        file_name = Some name;
	anchors = id_set_of_list (List.flatten (List.map all_anchors section.content)) }

    let flatten ?multi d =
      let xrefs = xrefs_of_sections single_page (List.rev d.rev_sections) in
      List.flatten
        (List.map
           (fun x ->
             hr () ::
             p [a ~a:[a_id x.self.section.anchor] []] ::
	     (match multi with
	     | None -> format_xref x
	     | Some m ->
		 let multi = URL.append_to_name x.self.section.anchor m in
		 format_xref ~multi x) ::
             x.self.section.content)
	   xrefs)

    let to_file ?multi name d =
      let flat_d =
	match multi with
	| None -> flatten d
	| Some name -> flatten ~multi:(URL.create ~name ()) d in
      body_to_file (URL.file_name (URL.create ~name ()))
	~title:d.title ~style:d.style flat_d

    let require_file_name p =
      match p.file_name with
      | None -> invalid_arg "require_file_name"
      | Some url -> URL.file_name url

    let to_files ?single prefix d =
      let xrefs = xrefs_of_sections (multi_page prefix) (List.rev d.rev_sections) in
      let rewrite_map = href_map (List.map (fun x -> x.self) xrefs) in
      let rewrite id =
	try URL.to_string (Href_Map.find id rewrite_map) with Not_found -> id in
      List.iter (fun x ->
        body_to_file (require_file_name x.self)
	  ~title:d.title
	  ~style:d.style
	  ~links:(link_of_rel_link `Start x.first @
		  link_of_rel_link `Prev x.prev @
		  link_of_rel_link `Next x.next)
          ((match single with
	    | None -> format_xref x
	    | Some s -> format_xref ~single:(URL.create ~name:s ()) x) :: hr () ::
	   List.map (rewrite_hrefs rewrite) x.self.section.content)) xrefs

(*
   let frameset_section =
   html ~a:[a_xmlns `W3_org_1999_xhtml; a_xml_lang "en"]
   (head (title main_title) [main_style ()])
   (frameset
   ~a:[a_fs_rows [`Pixels 150; `Relative 1]]
   ~noframes:(noframes (main_body ~standalone:false))
   (frame ~a:[a_scrolling `No] ~src:nav_url ())
   [frame ~a:[a_frame_id "main"] ~src:main_url ()])

 *)

  end
