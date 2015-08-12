(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2014
 * Vincent Balat
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

{shared{
val to_signal : init:'a -> 'a React.S.t Lwt.t -> 'a React.S.t
}}

{server{
module SharedReact : sig
  module S : sig
    include Eliom_csreact_sigs.S
    val create :
      ?default:'a ->
      ?reset_default:bool ->
      'a ->
      'a t * (?step:React.step -> 'a -> unit) Eliom_lib.shared_value
  end
end

module SharedReactiveData : sig
  module RList : Eliom_csreact_sigs.RLIST
    with type 'a signal := 'a SharedReact.S.t
end
}}

{client{
module SharedReact : sig
  module S : Eliom_csreact_sigs.S with type 'a t = 'a React.S.t
end

module SharedReactiveData : sig
  module RList : Eliom_csreact_sigs.RLIST
    with type 'a t = 'a ReactiveData.RList.t
     and type 'a signal := 'a SharedReact.S.t
end
}}
