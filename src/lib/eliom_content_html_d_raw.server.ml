(* This is [Eliom_content.Xml] adapted such that request nodes are produced *)
module Xml' = struct
  include Eliom_content_xml.Xml

  let make elt = make_request_node (make elt)

  let empty () = make Empty

  let comment c = make (Comment c)
  let pcdata d = make (PCDATA d)
  let encodedpcdata d = make (EncodedPCDATA d)
  let entity e = make (Entity e)

  let leaf ?(a = []) name =  make (Leaf (name, a))
  let node ?(a = []) name children = make (Node (name, a, children))
end

include Html_f.Make(Xml')(Eliom_content_svg_d_raw)
