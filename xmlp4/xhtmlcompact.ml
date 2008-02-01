
let ocsigenadv = ""
let str_formatter = Buffer.create 128
let pp_print_string = Buffer.add_string
let pp_open_tbox _ () = ()
let pp_force_newline _ () = ()
let pp_print_tbreak _ _ _ = ()
let pp_close_tbox _ _ = ()
let pp_set_margin _ _ = ()
let flush_str_formatter () =
  let s = Buffer.contents str_formatter in Buffer.clear str_formatter; s

INCLUDE "xhtmlpretty_base.ml"

(*****************************************************************************)
(* print to Ocsigen's streams *)

let x_stream, xh_stream =

  let aux ~width ~encode ?(html_compat = false)
      blocktags semiblocktags arbre cont =
    let endemptytag = if html_compat then ">" else "/>" in
    let rec xh_print_attrs encode attrs cont = match attrs with
    | [] -> cont ();
    | attr::queue ->
        (Ocsistream.cont (" "^XML.attrib_to_string encode attr)) (fun () ->
        xh_print_attrs encode queue cont)

    and xh_print_text texte i is_first cont =
      (Ocsistream.cont texte) cont

    and xh_print_closedtag encode tag attrs i is_first cont =
      (Ocsistream.cont ("<"^tag)) (fun () ->
      xh_print_attrs encode attrs (fun () ->
      (Ocsistream.cont endemptytag) cont))

    and xh_print_inlinetag encode tag attrs taglist i is_first cont =
      (Ocsistream.cont ("<"^tag)) (fun () ->
      xh_print_attrs encode attrs (fun () ->
      (Ocsistream.cont ">") (fun () ->
      xh_print_taglist taglist 0 false false (fun () ->
      (Ocsistream.cont ("</"^tag^">") cont)))))

    and xh_print_blocktag encode tag attrs taglist i cont =
      if taglist = []
      then xh_print_closedtag encode tag attrs i true cont
      else begin
        (Ocsistream.cont ("<"^tag)) (fun () ->
        xh_print_attrs encode attrs (fun () ->
        (Ocsistream.cont ">") (fun () ->
        xh_print_taglist_removews taglist (i+1) true (fun () ->
        (Ocsistream.cont ("</"^tag^">") cont)))))

      end

    and xh_print_semiblocktag encode tag attrs taglist i cont =
      if taglist = []
      then xh_print_closedtag encode tag attrs i true cont
      else begin
        (Ocsistream.cont ("<"^tag)) (fun () ->

        xh_print_attrs encode attrs (fun () ->
        (Ocsistream.cont ">") (fun () ->

        xh_print_taglist taglist 0 false false (fun () ->

        (Ocsistream.cont ("</"^tag^">") cont)))))

      end

    and xh_print_taglist_removews taglist i is_first cont =
      match taglist with
      | (Whitespace s)::l -> xh_print_taglist_removews l i is_first cont
      | l -> xh_print_taglist l i is_first true cont


    and print_nodes ws1 name xh_attrs xh_taglist ws2 queue i is_first removetailingws cont =
      if xh_taglist = []
      then begin
        xh_print_closedtag encode name xh_attrs i is_first (fun () ->
        xh_print_taglist queue i false removetailingws cont)
      end
      else begin
        (fun cont ->
          if (List.mem name blocktags)
          then xh_print_blocktag encode name xh_attrs xh_taglist i cont
          else
            (if (List.mem name semiblocktags)
            then xh_print_semiblocktag encode name xh_attrs xh_taglist i cont
            else begin
              xh_print_text (encode ws1) i is_first (fun () ->
              xh_print_inlinetag encode name xh_attrs xh_taglist i is_first (fun () ->
              xh_print_text (encode ws2) i is_first cont))
            end))
        (fun () -> xh_print_taglist queue i false removetailingws cont)
      end

    and xh_print_taglist taglist i is_first removetailingws cont =
      match taglist with

      | [] -> cont ()

      | (Comment texte)::queue ->
          xh_print_text ("<!--"^(encode texte)^"-->") i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

      | (Entity e)::queue ->
          xh_print_text ("&"^e^";") i is_first (* no encoding *)
          (fun () -> xh_print_taglist queue i false removetailingws cont)

      | (PCDATA texte)::queue ->
          xh_print_text (encode texte) i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

      | (EncodedPCDATA texte)::queue ->
          xh_print_text texte i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

      | (Whitespace _)::(Element ("hr",xh_attrs,[]))::(Whitespace _)::queue
      | (Element ("hr",xh_attrs,[]))::(Whitespace _)::queue
      | (Whitespace _)::(Element ("hr",xh_attrs,[]))::queue
      | (Element ("hr",xh_attrs,[]))::queue ->
          xh_print_closedtag id "hr" xh_attrs i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

      | (Element (name, xh_attrs, []))::queue ->
          xh_print_closedtag id name xh_attrs i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

              (* Balises de presentation, type inline *)
      | (Element (name, xh_attrs, xh_taglist))::queue ->
          xh_print_inlinetag id name xh_attrs xh_taglist i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

              (* Balises de type block *)
      | (Whitespace _)::(BlockElement (name,xh_attrs,xh_taglist))::(Whitespace _)::queue
      | (BlockElement (name,xh_attrs,xh_taglist))::(Whitespace _)::queue
      | (Whitespace _)::(BlockElement (name,xh_attrs,xh_taglist))::queue
      | (BlockElement (name,xh_attrs,xh_taglist))::queue ->
          xh_print_blocktag id name xh_attrs xh_taglist i
          (fun () -> xh_print_taglist queue i false removetailingws cont)

              (* Balises de type "semi block", for ex <pre> *)
      | (Whitespace _)::(SemiBlockElement (name,xh_attrs,xh_taglist))::(Whitespace _)::queue
      | (SemiBlockElement (name,xh_attrs,xh_taglist))::(Whitespace _)::queue
      | (Whitespace _)::(SemiBlockElement (name,xh_attrs,xh_taglist))::queue
      | (SemiBlockElement (name,xh_attrs,xh_taglist))::queue ->
          xh_print_semiblocktag id name xh_attrs xh_taglist i
          (fun () -> xh_print_taglist queue i false removetailingws cont)

              (* Nodes and Leafs *)
      | (Whitespace ws1)::(Node (name,xh_attrs,xh_taglist))::(Whitespace ws2)::queue ->
          print_nodes ws1 name xh_attrs xh_taglist ws2 queue i is_first removetailingws cont

      | (Node (name,xh_attrs,xh_taglist))::(Whitespace ws2)::queue ->
          print_nodes "" name xh_attrs xh_taglist ws2 queue i is_first removetailingws cont

      | (Whitespace ws1)::(Node (name,xh_attrs,xh_taglist))::queue ->
          print_nodes ws1 name xh_attrs xh_taglist "" queue i is_first removetailingws cont

      | (Node (name,xh_attrs,xh_taglist))::queue ->
          print_nodes "" name xh_attrs xh_taglist "" queue i is_first removetailingws cont

      | (Whitespace ws1)::(Leaf (name,xh_attrs))::(Whitespace ws2)::queue ->
          print_nodes ws1 name xh_attrs [] ws2 queue i is_first removetailingws cont

      | (Leaf (name,xh_attrs))::(Whitespace ws2)::queue ->
          print_nodes "" name xh_attrs [] ws2 queue i is_first removetailingws cont

      | (Whitespace ws1)::(Leaf (name,xh_attrs))::queue ->
          print_nodes ws1 name xh_attrs [] "" queue i is_first removetailingws cont

      | (Leaf (name,xh_attrs))::queue ->
          print_nodes "" name xh_attrs [] "" queue i is_first removetailingws cont

            (* Whitespaces *)
      | (Whitespace(texte))::queue ->
          xh_print_text (encode texte) i is_first
          (fun () -> xh_print_taglist queue i false removetailingws cont)

      | Empty::queue ->
          xh_print_taglist queue i false removetailingws cont



    in
    xh_print_taglist [arbre] 0 true false cont
  in
  ((fun ?(width = 132) ?(encode = encode_unsafe)
      ?html_compat blocktags semiblocktags
      doctype foret ->

         (List.fold_right
             (fun arbre cont () ->
               aux ?width ?encode ?html_compat
                 blocktags semiblocktags arbre cont)
             foret

         (fun () -> Ocsistream.empty None))),


   (fun ?(width = 132) ?(encode = encode_unsafe)
       ?html_compat blocktags semiblocktags
       doctype arbre ->

        Ocsistream.cont doctype
        (fun () -> Ocsistream.cont ocsigenadv
        (fun () ->

          aux ?width ?encode ?html_compat
           blocktags semiblocktags arbre

           (fun () -> Ocsistream.empty None)))))

let xhtml_stream ?(version=`XHTML_01_01) ?width ?encode ?html_compat arbre =
  Ocsistream.make
    (fun () ->
      xh_stream ?width ?encode ?html_compat
        blocktags semiblocktags
        (XHTML.M.doctype version) (XHTML.M.toelt arbre))

let xhtml_list_stream ?(version=`XHTML_01_01)
    ?width ?encode ?html_compat foret =
  Ocsistream.make
    (fun () ->
      x_stream ?width ?encode ?html_compat
        blocktags semiblocktags (XHTML.M.doctype version)
        (XHTML.M.toeltl foret) ())
