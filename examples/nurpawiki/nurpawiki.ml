(* Ocsigen
 * http://www.ocsigen.org
 * Module nurpawiki.ml
 * Copyright (C) 2007 Janne Helsten
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception; 
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open XHTML.M
open Eliom
open Eliom.Xhtml

open Simplexmlparser.ExprOrPatt
open Lwt

module P = Printf

let (>>) f g = g f

let wiki_view_page = new_service [] (suffix (string "p")) ()
let wiki_edit_page = new_service ["edit"] (string "p") ()
let wiki_start = Redirections.register_new_service [] unit 
    (fun sp _ _ -> return (make_string_uri wiki_view_page sp "WikiStart"))

let finally handler f x =
  let r = (
    try
      f x
    with
      e -> handler(); raise e
  ) in
  handler();
  r

let fold_read_lines f accum inchnl = 
  let line () = 
    try Some (input_line inchnl)
    with End_of_file -> None in
  let rec loop accum =
    match line () with
    | Some e -> loop (f accum e)
    | None -> accum in
  loop accum 

let with_open_out fname f = 
  let inchnl = open_out fname in
  finally 
    (fun () -> close_out inchnl)
    (fun chnl -> f chnl) inchnl

let with_open_in fname f = 
  let inchnl = open_in fname in
  finally 
    (fun () -> close_in inchnl)
    (fun chnl -> f chnl) inchnl

let wiki_file_dir = 
  let rec find_wikidata = function
      PLCons 
        (EPanytag 
           ("wikidata", 
            (PLCons
               ((EPanyattr (EPVstr "dir", EPVstr s)), PLEmpty)),_),ll) ->
          s
    | PLCons ((EPcomment _), l) | PLCons ((EPwhitespace _), l) ->
        find_wikidata l 
    | PLEmpty | PLCons _->
        assert false in
  let c = Eliom.get_config () in
  find_wikidata c

let wiki_page_filename page =
  wiki_file_dir ^ "/" ^ page ^ ".wiki"

let wiki_page_exists page =
  Sys.file_exists (wiki_page_filename page)

let save_wiki_page page text =
  with_open_out
    (wiki_page_filename page)
    (fun chnl -> output_string chnl text)

let load_wiki_page page =
  with_open_in 
    (wiki_page_filename page)
    (fun chnl ->
       List.rev (fold_read_lines (fun acc line -> line::acc) [] chnl))

let h1_re = Str.regexp "^=\\(.*\\)=\\([ \n\r]*\\)?$"
let h2_re = Str.regexp "^==\\(.*\\)==\\([ \n\r]*\\)?$"
let h3_re = Str.regexp "^===\\(.*\\)===\\([ \n\r]*\\)?$"
let list_re = Str.regexp "^[ ]?\\([*]+\\) \\(.*\\)\\([ \n\r]*\\)?$"

let is_list s = 
  Str.string_match list_re s 0

let take_while pred lines =
  let rec loop acc = function 
      (x::xs) as lst -> 
        if pred x then
          loop (x::acc) xs
        else 
          (lst, List.rev acc)
    | [] ->
        ([], List.rev acc) in
  loop [] lines

let accepted_chars_ = "a-zA-Z\128-\2550-9_!\"§°#%&/()=?+.,;:{}'@\\$\\^\\*`´<>"
let accepted_chars_sans_ws = "["^accepted_chars_^"-]+"
let accepted_chars = "["^accepted_chars_^" -]+"
let text_re = Str.regexp ("\\("^accepted_chars_sans_ws^"\\)")
let wikilink_re = Str.regexp "\\([A-Z][a-z]+\\([A-Z][a-z]+\\)+\\)"

let wikilinkanum_re = 
  Str.regexp ("\\(\\[\\(wiki\\|file\\|http\\):\\("^accepted_chars_sans_ws^
                "\\)[ ]+\\("^accepted_chars^"\\)\\]\\)")

let wikilinkanum_no_text_re = 
  Str.regexp ("\\(\\[\\(wiki\\|file\\|http\\):\\("^accepted_chars_sans_ws^"\\)\\]\\)")

let open_pre_re = Str.regexp "^\\(<pre>\\|{{{\\)[ \n\r]+$"
let close_pre_re = Str.regexp "^\\(</pre>\\|}}}\\)[ \n\r]+$"

let translate_list items =

  let add_ul t lst = 
    t @ [ul (List.hd lst) (List.tl lst)] in

  let rec loop = function
      ((nesting1,text1)::(nesting2,text2)::xs) as lst ->
        if nesting1 = nesting2 then
          (li text1)::loop (List.tl lst)
        else if nesting1 < nesting2 then (* enter *)
          let (next_same_level,same_or_higher) = 
            take_while (fun (n,_) -> n >= nesting2) (List.tl lst) in
          (li (add_ul text1 (loop same_or_higher)))::loop next_same_level
        else (* leave *)
          loop (List.tl lst)
    | (nesting,text)::[] ->
        [(li text)]
    | [] -> [] in
  let list_items = loop items in
  ul (List.hd list_items) (List.tl list_items)

let parse_lines sp lines =

  let wikilink scheme page text = 
    if scheme = "wiki" || scheme = "" then
      let t = if text = "" then page else text in
      if wiki_page_exists page then
        a wiki_view_page sp [pcdata t] page
      else 
        a ~a:[a_class ["missing_page"]] wiki_view_page sp [pcdata t] 
          page
    else (* External link *)
      let url = scheme^":"^page in
      let t = if text = "" then url else text in
      a (new_external_service 
           ~url:[url]
           ~get_params:(suffix (all_suffix "suff"))
           ~post_params:unit ()) sp [pcdata t; ext_img] [page] in
    
  let rec parse_text acc s =
    let len = String.length s in
    let add_html html_acc html =
      html::html_acc in
    let rec loop acc charpos =
      if charpos >= len then
        acc
      else 
        if s.[charpos] = '\t' then 
          let m = "\t" in
          loop (add_html acc (pcdata m)) (charpos+1)
        else if s.[charpos] = ' ' then 
          let m = " " in
          loop (add_html acc (pcdata m)) (charpos+1)
        else if s.[charpos] = '\r' || s.[charpos] = '\n' then
          acc
        else if Str.string_match wikilink_re s charpos then
          let m = Str.matched_group 1 s in
          loop (add_html acc (wikilink "" m m)) (charpos+(String.length m))
        else if Str.string_match wikilinkanum_re s charpos then
          let scheme = Str.matched_group 2 s in
          let page = Str.matched_group 3 s in
          let text = Str.matched_group 4 s in
          let fm_len = String.length (Str.matched_group 1 s) in
          loop (add_html acc (wikilink scheme page text)) (charpos+fm_len)
        else if Str.string_match wikilinkanum_no_text_re s charpos then
          let scheme = Str.matched_group 2 s in
          let page = Str.matched_group 3 s in
          let text = "" in
          let fm_len = String.length (Str.matched_group 1 s) in
          loop (add_html acc (wikilink scheme page text)) (charpos+fm_len)
        else if Str.string_match text_re s charpos then
          let m = Str.matched_group 1 s in
          loop (add_html acc (pcdata m)) (charpos+(String.length m))
        else
          begin
            let s = (String.sub s charpos ((String.length s)-charpos)) in
            add_html acc
              (span
                 [span ~a:[a_class ["error"]] 
                    [pcdata "WIKI SYNTAX ERROR IN INPUT: "];
                  pcdata s])
          end
    in
    List.rev (loop acc 0) in
  
  let rec loop acc = function
      (x::xs) as lst ->
        if Str.string_match h3_re x 0 then
          loop ((h3 [pcdata (Str.matched_group 1 x)])::acc) xs
        else if Str.string_match h2_re x 0 then
          loop ((h2 [pcdata (Str.matched_group 1 x)])::acc) xs
        else if Str.string_match h1_re x 0 then
          loop ((h1 [pcdata (Str.matched_group 1 x)])::acc) xs
        else if is_list x then
          (* Grab all lines starting with '*': *)
          let (after_bullets,bullets) =
            take_while is_list lst in
          let list_items = 
            List.map
              (fun e ->
                 if is_list e then
                   let n_stars = String.length (Str.matched_group 1 e) in
                   (n_stars, parse_text [] (Str.matched_group 2 e))
                 else 
                   assert false) bullets in
          loop ((translate_list list_items)::acc) after_bullets
        else if Str.string_partial_match open_pre_re x 0 then
          (* Handle <pre>..</pre>, {{{..}}} *)
          let (after_pre,contents) =
            take_while 
              (fun x -> not (Str.string_partial_match close_pre_re x 0))
              lst in
          let p = 
            (pre [pcdata (String.concat "\n" (List.tl contents))]) in
          loop (p::acc) (List.tl after_pre)
        else 
          loop ((p (parse_text [] x))::acc) xs
    | [] -> List.rev acc in

  loop [] lines

let wikiml_to_html sp page =
  if wiki_page_exists page then
    load_wiki_page page >> parse_lines sp
  else
    []

(* Use this as the basis for all pages.  Includes CSS etc. *)
let html_stub sp body_html =
  return 
    (html 
       (head (title (pcdata "")) 
          [css_link (make_uri (static_dir sp) sp ["style.css"])])
       (body body_html))

let wiki_page_menu_html sp page content =
  [div ~a:[a_id "navbar"]
     [div ~a:[a_id "akmenu"]
        [p
           [span ~a:[a_class ["nwikilogo"]] [(pcdata "NurpaWiki")];
            a wiki_view_page 
              ~a:[a_accesskey 'h'; a_class ["ak"]] sp 
              [pcdata "Home"] "WikiStart";
            a wiki_edit_page ~a:[a_accesskey 'e'; a_class ["ak"]] sp 
              [pcdata "Edit page"] page; br ()]]];
   div ~a:[a_id "content"]
     content]

let wiki_page_contents_html sp page ?(content=[]) () =
  wiki_page_menu_html sp page (content @ wikiml_to_html sp page)

let view_page sp page =
  html_stub sp
    (wiki_page_contents_html sp page ())

(* Save page as a result of /edit?p=Page *)
let service_save_page_post =
  register_new_post_service
    ~fallback:wiki_view_page
    ~post_params:(string "value")
    (fun sp page value -> 
       (* Save wiki page from POST value: *)
       save_wiki_page page value;
       view_page sp page)
    
(* /edit?p=Page *)
let _ =
  register wiki_edit_page
    (fun sp page () -> 
       let wikitext = 
         if wiki_page_exists page then
           String.concat "\n" (load_wiki_page page)
         else 
           "" in
       let f =
         post_form service_save_page_post sp
           (fun chain -> 
              [(p [submit_input "Save"; br ();
                   textarea chain ~rows:30 ~cols:80 (pcdata wikitext)])])
           page in
       html_stub sp
         (wiki_page_contents_html sp page ~content:[f] ()))

(* /view?p=Page *)
let _ = 
  register wiki_view_page
    (fun sp page () ->
       if not (wiki_page_exists page) then
         let f = 
           a wiki_edit_page sp [pcdata "Create new page"] page in
         html_stub sp
           (wiki_page_menu_html sp page [f])
       else
         view_page sp page)
