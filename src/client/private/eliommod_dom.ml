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
    List.iter (Dom.appendChild copy) child_copies;
    Some copy
  in
  match aux e with
    | None -> error "copy_element"
    | Some e -> e
