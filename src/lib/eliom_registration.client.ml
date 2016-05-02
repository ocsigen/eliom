module type Base = sig
  type return = Eliom_service.non_ocaml
end

module Base = struct
  type return = Eliom_service.non_ocaml
end

module Block5 = Base
module Html_text = Base
module CssText = Base
module Text = Base
module String = Base

module Unit = Base

module String_redirection = Base

module Any = Base
module Streamlist = Base

module Ocaml = struct
  type 'a return = 'a Eliom_service.ocaml
end

module Redirection = struct
  type 'a return = 'a
end

module Html_reg_base = struct

  type page = Html_types.html Eliom_content.Html.elt
  type options = unit

  let send ?options:_ page =
    Eliom_client.set_content_local
      (Eliom_content.Html.To_dom.of_element page)

end

module Html = Eliom_mkreg.Make(Html_reg_base)

module Action_reg_base = struct

  type page = unit
  type options = [ `Reload | `NoReload ]

  let send ?(options : [ `Reload | `NoReload ] option) _ =
    match !Eliom_client.reload_function, options with
    | Some f, Some `Reload ->
      f () ()
    | _, _ ->
      Lwt.return ()

end

module Action = Eliom_mkreg.Make(Action_reg_base)
