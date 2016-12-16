open Eliom_lib
open Eliom_content_xml

module Make
    (Kind : sig
       type +'a elt
       val tot: Xml.elt -> 'a elt
       val toelt: 'a elt -> Xml.elt
     end)
    (To_dom : sig
       val of_element: 'a Kind.elt -> Dom_html.element Js.t
     end)
    (Of_dom : sig
       val of_element: Dom_html.element Js.t -> 'a Kind.elt
     end)
    (Id: sig
       type 'a id
       val get_element': 'a id -> Dom.node Js.t
     end)
    (Ns: sig
       val content_ns : Eliom_client_core.content_ns
     end) = struct

    let get_node elt = ((To_dom.of_element elt) :> Dom.node Js.t)
    let get_unique_node context (elt: 'a Kind.elt) : Dom.node Js.t =
      match Xml.get_node (Kind.toelt elt) with
      | Xml.DomNode node -> node
      | Xml.ReactNode s -> get_node elt
      | Xml.ReactChildren (node,rl) -> get_node elt
      | Xml.TyXMLNode desc ->
        let elt' = Kind.toelt elt in
          match Xml.get_node_id elt' with
          | Xml.NoId ->
            Lwt_log.raise_error_f ~section:Lwt_log.eliom
              ~inspect:(Eliom_client_core.rebuild_node'
                          Ns.content_ns (Kind.toelt elt))
              "Cannot call %s on an element with functional semantics" context
          | _ -> get_node elt

    let get_unique_elt name elt : Dom_html.element Js.t =
      Js.Opt.case
        (Dom_html.CoerceTo.element (get_unique_node name elt))
        (fun () ->
           Lwt_log.raise_error_f ~section:Lwt_log.eliom
             ~inspect:(Eliom_client_core.rebuild_node'
                         Ns.content_ns (Kind.toelt elt))
             "Cannot call %s on a node which is not an element" name;
        )
        id

    let raw_appendChild ?before node elt2 =
      match before with
      | None -> ignore(node##(appendChild (get_node elt2)))
      | Some elt3 ->
          let node3 = get_unique_node "appendChild" elt3 in
          ignore(node##(insertBefore (get_node elt2) (Js.some node3)))


    let raw_appendChildren ?before node elts =
      match before with
      | None ->
          List.iter (fun elt2 -> ignore(node##(appendChild (get_node elt2)))) elts
      | Some elt3 ->
          let node3 = get_unique_node "appendChild" elt3 in
          List.iter (fun elt2 ->
            ignore(node##(insertBefore (get_node elt2) (Js.some node3)))) elts

    let raw_removeChild node1 elt2 =
      let node2 = get_unique_node "removeChild" elt2 in
      ignore(node1##(removeChild node2))

    let raw_replaceChild node1 elt2 elt3 =
      let node3 = get_unique_node "replaceChild" elt3 in
      ignore(node1##(replaceChild (get_node elt2) node3))

    let raw_removeChildren node =
      let childrens = Dom.list_of_nodeList (node##.childNodes) in
      List.iter (fun c -> ignore(node##(removeChild c))) childrens

    let raw_replaceChildren node elts =
      raw_removeChildren node;
      List.iter (fun elt -> ignore(node##(appendChild (get_node elt)))) elts

    let nth elt n =
      let node = get_unique_node "nth" elt in
      let res = Js.Opt.bind (node##.childNodes##(item n)) (fun node ->
        Js.Opt.map (Dom.CoerceTo.element node) (fun node ->
          Of_dom.of_element (Dom_html.element node)
        )
      ) in
      Js.Opt.to_option res

    let childLength elt =
      let node = get_unique_node "childLength" elt in
      node##.childNodes##.length

    let appendChild ?before elt1 elt2 =
      let node = get_unique_node "appendChild" elt1 in
      raw_appendChild ?before node elt2

    let appendChildren ?before elt1 elts =
      let node = get_unique_node "appendChildren" elt1 in
      raw_appendChildren ?before node elts

    let removeChild elt1 elt2 =
      let node1 = get_unique_node "removeChild" elt1 in
      raw_removeChild node1 elt2

    let removeSelf elt =
      let node = get_unique_node "removeSelf" elt in
      let res = Js.Opt.bind (node##.parentNode) (fun node ->
        Js.Opt.map (Dom.CoerceTo.element node) (fun node ->
          Of_dom.of_element (Dom_html.element node)
        )
      ) in
      Js.Opt.iter res (fun p -> removeChild p elt)

    let insertFirstChild p c =
      let before = nth p 0 in
      appendChild ?before p c

    let replaceChild elt1 elt2 elt3 =
      let node1 = get_unique_node "replaceChild" elt1 in
      raw_replaceChild node1 elt2 elt3

    let removeChildren elt =
      let node = get_unique_node "removeChildren" elt in
      raw_removeChildren node

    let replaceChildren elt elts =
      let node = get_unique_node "replaceChildren" elt in
      raw_replaceChildren node elts

    let childNodes elt =
      let node = get_unique_node "childNodes" elt in
      Dom.list_of_nodeList (node##.childNodes)

    let rec filterElements coerce nodes = match nodes with
      | [] -> []
      | node :: nodes ->
        let elts = filterElements coerce nodes in
        Js.Opt.case
          (coerce node)
          (fun () -> elts)
          (fun elt -> elt :: elts)

    let childElements elt =
      let node = get_unique_node "childElements" elt in
      filterElements Dom.CoerceTo.element (Dom.list_of_nodeList (node##.childNodes))

    let children elt =
      let node = get_unique_node "children" elt in
      List.map Of_dom.of_element
        (filterElements Dom_html.CoerceTo.element
           (Dom.list_of_nodeList (node##.childNodes)))

    let parentNode elt =
      let node = get_unique_node "parentNode" elt in
      let res = Js.Opt.bind
          (node##.parentNode)
          (fun node ->
             Js.Opt.map (Dom.CoerceTo.element node)
               (fun node -> Of_dom.of_element (Dom_html.element node)))
      in
      Js.Opt.to_option res

    let nextSibling elt =
      let node = get_unique_node "nextSibling" elt in
      let res = Js.Opt.bind (node##.nextSibling)
          (fun node ->
             Js.Opt.map (Dom.CoerceTo.element node)
               (fun node -> Of_dom.of_element (Dom_html.element node)))
      in
      Js.Opt.to_option res

    let previousSibling elt =
      let node = get_unique_node "previousSibling" elt in
      let res = Js.Opt.bind (node##.previousSibling)
          (fun node ->
             Js.Opt.map (Dom.CoerceTo.element node)
               (fun node -> Of_dom.of_element (Dom_html.element node)))
      in
      Js.Opt.to_option res

    let insertBefore ~before elt =
      Eliom_lib.Option.iter
        (fun parent -> appendChild ~before parent elt)
        (parentNode before)

    let insertAfter ~after elt =
      Eliom_lib.Option.iter
        (fun parent ->
           let before = nextSibling after in
           appendChild ?before parent elt)
        (parentNode after)

    let replaceSelf elt1 elt2 =
      Eliom_lib.Option.iter
        (fun parent -> replaceChild parent elt2 elt1)
        (parentNode elt1)

    module RawNamed = struct

      let appendChild ?before id1 elt2 =
        let node = Id.get_element' id1 in
        raw_appendChild ?before node elt2

      let appendChildren ?before id1 elts =
        let node = Id.get_element' id1 in
        raw_appendChildren ?before node elts

      let removeChild id1 elt2 =
        let node1 = Id.get_element' id1 in
        raw_removeChild node1 elt2

      let replaceChild id1 elt2 elt3 =
        let node1 = Id.get_element' id1 in
        raw_replaceChild node1 elt2 elt3

      let removeChildren id =
        let node = Id.get_element' id in
        raw_removeChildren node

      let replaceChildren id elts =
        let node = Id.get_element' id in
        raw_replaceChildren node elts

    end

  module Class = struct

    let contain elt class_name =
      let elt = get_unique_elt "Class.contain" elt in
      let class_name = Js.string class_name in
      let class_list = elt##.classList in
      Js.to_bool (class_list##(contains class_name))

    let add_raw elt class_name =
      let class_name = Js.string class_name in
      let class_list = elt##.classList in
      if Js.to_bool (class_list##(contains class_name))
      then ()
      else class_list##(add class_name)

    let add elt class_name =
      let elt = get_unique_elt "Class.add" elt in
      add_raw elt class_name

    let adds elt class_list =
      let elt = get_unique_elt "Class.adds" elt in
      List.iter (fun class_name -> add_raw elt class_name) class_list

    let remove_raw elt class_name =
      let class_name = Js.string class_name in
      let class_list = elt##.classList in
      if Js.to_bool (class_list##(contains class_name))
      then class_list##(remove class_name)
      else ()

    let remove elt class_name =
      let elt = get_unique_elt "Class.remove" elt in
      remove_raw elt class_name

    let removes elt class_list =
      let elt = get_unique_elt "Class.removes" elt in
      List.iter (fun class_name -> remove_raw elt class_name) class_list

    let replace elt class_name_1 class_name_2 =
      let elt = get_unique_elt "Class.replace" elt in
      remove_raw elt class_name_1;
      add_raw elt class_name_2

    let clear elt =
      let elt = get_unique_elt "Class.clear" elt in
      let class_list = elt##.classList in
      let l = class_list##.length in
      for i = (l - 1) downto 0 do (* /!\ use downto because the list is re-ordered after each add/remove *)
        Js.Optdef.iter (class_list##(item i))
          (fun cl -> class_list##(remove cl))
      done

      let toggle elt cl1 =
        if contain elt cl1
        then remove elt cl1
        else add elt cl1
      let toggle2 elt cl1 cl2 =
          if contain elt cl1
          then replace elt cl1 cl2
          else replace elt cl2 cl1

    end

end
