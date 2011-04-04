
include Eliom_output_base

module type Base =
sig
  type return = Eliom_services.http
end

module Base =
struct
  type return = Eliom_services.http
end

module Html5 =
struct
  include Html5forms
  include Base
end

module Xhtml = Base
module Redirection = Base
module Blocks5 = Base
