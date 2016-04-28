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

module Action = struct

  type page    = unit
  type options = [`Reload | `NoReload]
  type return  = Eliom_service.non_ocaml

  let register
      ?app ?scope:_ ?options ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ~service ?error_handler:_
      f =
    (* The action has no reload function of its own
       (reset_reload_fun). If it did, Eliom_client.change_page would
       get us into an infinite loop. We rely on the reload_function of
       the previously-called service. *)
    Eliom_service.set_client_fun ?app ~service
      (fun g p ->
         lwt _ = f g p in
         match !Eliom_client.reload_function, options with
         | Some rf, (Some `Reload | None) ->
           rf () ()
         | _, _ ->
           Lwt.return ());
    Eliom_service.reset_reload_fun service

  let create
      ?app ?scope:_ ?options:_ ?charset:_ ?code:_ ?content_type:_
      ?headers:_ ?secure_session:_ ?https ?name ?csrf_safe ?csrf_scope
      ?csrf_secure ?max_use ?timeout ~meth ~id ?error_handler
      f =
    let service =
      Eliom_service.create
        ?name ?csrf_safe
        ?csrf_scope:(csrf_scope :> Eliom_common.user_scope option)
        ?csrf_secure ?max_use ?timeout ?https ~meth ~id ()
    in
    register ?app ~service f;
    service

end
