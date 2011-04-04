
include module type of Eliom_output_base

module type Base =
sig
  type return = Eliom_services.http
end

module Html5 :
sig
  include HTML5FORMSSIG
  include Base
end

module Xhtml : Base
module Redirection : Base
module Blocks5 : Base
