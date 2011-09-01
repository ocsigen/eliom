open Eliom_pervasives

(* We can't use Dom_html.document##head: it is not defined in ff3.6... *)
let get_head page =
  Js.Opt.get
    ((page##getElementsByTagName(Js.string "head"))##item(0))
    (fun () -> error "get_head")

let get_body page =
  Js.Opt.get
    ((page##getElementsByTagName(Js.string "body"))##item(0))
    (fun () -> error "get_body")

let iter_dom_array (f:'a -> unit)
    (a:<length : <get : int; ..> Js.gen_prop; item : int -> 'a Js.opt Js.meth; ..> Js.t) =
  let length = a##length in
  for i = 0 to length - 1 do
    Js.Opt.iter (a##item(i)) f;
  done

let copy_text t = Dom_html.document##createTextNode(t##data)

(* ie, ff3.6 and safari does not like setting innerHTML on html and
   head nodes: we need to rebuild the HTML dom tree from the XML dom
   tree received in the xhr *)


(* BEGIN IE<9 HACK:
   appendChild is broken in ie:
   see
     http://webbugtrack.blogspot.com/2009/01/bug-143-createtextnode-doesnt-work-on.html
     http://webbugtrack.blogspot.com/2007/10/bug-142-appendchild-doesnt-work-on.html

   This fix appending to script element.
   TODO: it is also broken when appending tr to tbody, need to find a solution
*)
let add_childrens (elt:Dom_html.element Js.t) (sons:Dom.node Js.t list) =
  try
    List.iter (Dom.appendChild elt) sons
  with
    | exn ->
      (* this code is ie only, there are no reason for an appendChild
	 to fail normally *)
      let concat l =
	let rec concat acc = function
	  | [] -> acc
	  | t::q ->
	    let txt =
	      match (Dom.nodeType t) with
		| Dom.Text t -> t
		| _ -> error "add_childrens: not text node in tag %s" (Js.to_string (elt##tagName)) in
	    concat (acc##concat(txt##data)) q
	in
	concat (Js.string "") l
      in
      match Dom_html.tagged elt with
	| Dom_html.Script elt ->
	  elt##text <- concat sons
	| Dom_html.Style elt ->
	  (* we need to append the style node to something. If we
	     don't do that the styleSheet field is not created if we.
	     And we can't do it by creating it with the ie specific
	     document.createStyleSheet: the styleSheet field is not
	     initialised and it can't be set either. *)
	  let d = Dom_html.createHead Dom_html.document in
	  Dom.appendChild d elt;
	  (Js.Unsafe.coerce elt)##styleSheet##cssText <- concat sons
	| _ -> debug_exn "add children: can't appendChild" exn; raise exn

(* END IE HACK *)

let copy_element (e:Dom.element Js.t) : Dom_html.element Js.t =
  let rec aux (e:Dom.element Js.t) =
    let copy = Dom_html.document##createElement(e##tagName) in
    let add_attribute a =
      Js.Opt.iter (Dom.CoerceTo.attr a)
        (* we don't use copy##attributes##setNameditem:
           in ie 9 it fail setting types of buttons... *)
        (fun a -> copy##setAttribute(a##name,a##value)) in
    iter_dom_array add_attribute (e##attributes);
    let child_copies = List.map_filter
      (fun child ->
        match Dom.nodeType child with
          | Dom.Text t ->
            Some (copy_text t:>Dom.node Js.t)
          | Dom.Element child ->
            (aux child:>Dom.node Js.t option)
          | _ ->
            None)
      (Dom.list_of_nodeList (e##childNodes)) in
    add_childrens copy child_copies;
    Some copy
  in
  match aux e with
    | None -> error "copy_element"
    | Some e -> e
