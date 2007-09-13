(* Ocsigen
 * http://www.ocsigen.org
 * Module miniwiki.ml
 * Copyright (C) 2007 Janne Hellsten
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
open Eliompredefmod.Xhtml
open Eliompredefmod
open Eliomservices
open Eliomparameters
open Eliomsessions

open Simplexmlparser
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
      [Element ("wikidata", [("dir", s)],_)] -> s
    | _ -> raise (Extensions.Error_in_config_file ("Unexpected content inside Miniwiki config"))
  in
  let c = get_config () in
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


let h1_re = Pcre.regexp "^=(.*)=([ \n\r]*)?$"
let h2_re = Pcre.regexp "^==(.*)==([ \n\r]*)?$"
let h3_re = Pcre.regexp "^===(.*)===([ \n\r]*)?$"
let list_re = Pcre.regexp "^[ ]?([*]+) (.*)([ \n\r]*)?$"

let match_pcre_option rex s =
  try Some (Pcre.extract ~rex s) with Not_found -> None

let is_list s = 
  match_pcre_option list_re s

let open_pre_re = Pcre.regexp "^(<pre>|{{{)[ \n\r]+$"
let close_pre_re = Pcre.regexp "^(</pre>|}}})[ \n\r]+$"

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


let comp_re = Pcre.regexp ~flags:[`ANCHORED]

let accepted_chars_ = "a-zA-Z\128-\2550-9_!\"§°#%&/\\(\\)=\\?\\+\\.,;:{}'@\\$\\^\\*`´<>"
let accepted_chars_sans_ws = "["^accepted_chars_^"-]+"
let accepted_chars = "["^accepted_chars_^" -]+"

let text_re = comp_re ("("^accepted_chars_sans_ws^")")
let wikilink_re = comp_re "([A-Z][a-z]+([A-Z][a-z]+)+)"

let wikilinkanum_re = 
  comp_re
    ("(\\[(wiki|file|http):("^accepted_chars_sans_ws^")[ ]+("^accepted_chars^")\\])")

let wikilinkanum_no_text_re = 
  comp_re ("(\\[(wiki|file|http):("^accepted_chars_sans_ws^")\\])")

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
        a ~a:[a_class ["missing_page"]] ~service:wiki_view_page ~sp [pcdata t] 
          page
    else (* External link *)
      let url = scheme^":"^page in
      let t = if text = "" then url else text in
      a (new_external_service 
           ~url:[url]
           ~get_params:unit
           ~post_params:unit ()) sp [pcdata t] () in

  let rec pcre_first_match str pos =
    let rec loop = function
        (rex,f)::xs ->
          (try Some (Pcre.extract ~rex ~pos str, f) with Not_found -> loop xs)
      | [] -> None in
    loop in
  
  (* Parse a line of text *)
  let rec parse_text acc s =

    let len = String.length s in
    let add_html html_acc html =
      html::html_acc in

    let parse_wikilink acc r charpos =
      (add_html acc (wikilink "" r.(1) r.(1)), charpos+(String.length r.(0))) in

    let parse_wikilinkanum acc r charpos =
      let scheme = r.(2) in
      let page = r.(3) in
      let text = r.(4) in
      let fm_len = String.length r.(0) in
      (add_html acc (wikilink scheme page text), charpos+fm_len) in

    let parse_wikilinkanum_no_text acc r charpos =
      let scheme = r.(2) in
      let page = r.(3) in
      let text = "" in
      let fm_len = String.length r.(0) in
      (add_html acc (wikilink scheme page text), charpos+fm_len) in

    let parse_text acc r charpos =
      (add_html acc (pcdata r.(1)), charpos+(String.length r.(0))) in
    
    let text_patterns = 
      [(wikilink_re, parse_wikilink);
       (wikilinkanum_re, parse_wikilinkanum);
       (wikilinkanum_no_text_re, parse_wikilinkanum_no_text);
       (text_re, parse_text)] in

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
        else
          begin
            match pcre_first_match s charpos text_patterns with
              Some (r,f) ->
                let (acc',charpos') = f acc r charpos in
                loop acc' charpos'
            | None ->
                let s = (String.sub s charpos ((String.length s)-charpos)) in
                add_html acc
                  (span
                     [span ~a:[a_class ["error"]] 
                        [pcdata "WIKI SYNTAX ERROR IN INPUT: "];
                      pcdata s])
          end
    in
    List.rev (loop acc 0) in
  
  (* Line-by-line wiki parser *)
  let rec loop acc = function
      (x::xs) as lst ->
        let parse_list r = 
          (* Grab all lines starting with '*': *)
          let (after_bullets,bullets) =
            take_while (fun e -> is_list e <> None) lst in
          let list_items = 
            List.map
              (fun e ->
                 match is_list e with
                   Some r ->
                     let n_stars = String.length r.(1) in
                     (n_stars, parse_text [] r.(2))
                 | None -> assert false) bullets in
          loop ((translate_list list_items)::acc) after_bullets in

        let parse_verbatim r =
          (* Handle <pre>..</pre>, {{{..}}} *)
          let (after_pre,contents) =
            take_while 
              (fun x -> match_pcre_option close_pre_re x = None)
              lst in
          let p = 
            (pre [pcdata (String.concat "\n" (List.tl contents))]) in
          loop (p::acc) (List.tl after_pre) in
          
        let wiki_pats = 
          [(h3_re, (fun r -> loop ((h3 [pcdata r.(1)])::acc) xs));
           (h2_re, (fun r -> loop ((h2 [pcdata r.(1)])::acc) xs));
           (h1_re, (fun r -> loop ((h1 [pcdata r.(1)])::acc) xs));
           (list_re, parse_list);
           (open_pre_re, parse_verbatim)] in
        begin
          match pcre_first_match x 0 wiki_pats with 
            Some (res, action) -> action res
          | None ->
              loop ((p (parse_text [] x))::acc) xs
        end
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
           [span ~a:[a_class ["nwikilogo"]] [(pcdata "MiniWiki")];
            a ~service:wiki_view_page 
              ~a:[a_accesskey 'h'; a_class ["ak"]] ~sp 
              [pcdata "Home"] "WikiStart";
            a ~service:wiki_edit_page ~a:[a_accesskey 'e'; a_class ["ak"]] ~sp 
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
              [(p [string_input ~input_type:`Submit ~value:"Save" (); br ();
                   textarea ~name:chain ~rows:30 ~cols:80 
                     ~value:(pcdata wikitext) ()])])
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
