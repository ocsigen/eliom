
open Eliom_content_core

module XML = XML

module SVG = struct

  module F = SVG.F
  module D = SVG.D
  module Id = SVG.Id

  type 'a elt = 'a F.elt
  type 'a attrib = 'a F.attrib
  type uri = F.uri

end

module HTML5 = struct

  module F = HTML5.F
  module D = HTML5.D

  module Id = HTML5.Id

  type 'a elt = 'a F.elt
  type 'a attrib = 'a F.attrib
  type uri = F.uri

  module Of_dom = HTML5.Of_dom
end
