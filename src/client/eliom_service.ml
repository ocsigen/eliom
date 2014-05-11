(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

include Eliom_service_base

type http_service = [ `Http ]
type appl_service = [ `Appl ]
type 'a ocaml_service = [ `Ocaml of 'a ]

type non_ocaml_service = [ appl_service | http_service ]

type 'rt rt = unit
let rt = ()

module MakeBase = struct
  let service = service
  let post_service = post_service
  let put_service = put_service
  let delete_service = delete_service
  let coservice = coservice
  let post_coservice = post_coservice
  let put_coservice = put_coservice
  let delete_coservice = delete_coservice
  let coservice' = coservice'
  let post_coservice' = post_coservice'
  let put_coservice' = put_coservice'
  let delete_coservice' = delete_coservice'
  let external_service = external_service
  let external_post_service = external_post_service
  let external_put_service = external_put_service
  let external_delete_service = external_delete_service
end

module Unsafe = struct
  include MakeBase
end
module App = struct
  include MakeBase
end
module Ocaml = struct
  include MakeBase
end
module Http = struct
  include MakeBase
end

let xhr_with_cookies s =
  if is_external s then
    None
  else
    match s.send_appl_content with
    | XAlways -> Some None
    | XNever -> None
    | XSame_appl (appl, _) when Some appl <> Eliom_process.get_application_name () -> None
    | XSame_appl (_, tmpl) -> Some tmpl
