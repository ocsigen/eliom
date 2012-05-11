
open Eliom_content_core

module XML = XML

module SVG = struct

  module F = SVG.F
  module D = SVG.D

  module Id = SVG.Id

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri

  module Printer = SVG.Printer

end

module HTML5 = struct

  module F = HTML5.F
  module D = HTML5.D

  module Id = HTML5.Id

  module Printer = HTML5.Printer

  type +'a elt = 'a F.elt
  type +'a attrib = 'a F.attrib
  type uri = F.uri
end

module XHTML = struct

  module F = XHTML.F
  module F_01_00 = XHTML.F_01_00
  module F_01_01 = XHTML.F_01_01
  module Printer = XHTML.Printer
  module Printer_01_00 = XHTML.Printer_01_00
  module Printer_01_01 = XHTML.Printer_01_01

end
