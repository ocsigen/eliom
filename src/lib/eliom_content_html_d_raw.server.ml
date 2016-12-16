(* This is [Eliom_content.Xml] adapted such that request nodes are produced *)
module Xml' = struct
  include Eliom_content_xml.Xml

  let make elt = make_request_node (make elt)
  let make_lazy elt = make_request_node (make_lazy elt)

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
  let lazy_node ?(a = []) name children =
    make_lazy (Eliom_lazy.from_fun (fun () -> (Node (name, a, Eliom_lazy.force children))))

end

include Html_f.Make(Xml')(Eliom_content_svg_d_raw)
