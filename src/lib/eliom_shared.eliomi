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
module React : sig

  module S : sig

    include Eliom_shared_sigs.S

    val create :
      ?default :
        ('a t Eliom_lib.client_value *
         (?step:React.step -> 'a -> unit) Eliom_lib.client_value) ->
      ?reset_default:bool ->
      'a ->
      'a t * (?step:React.step -> 'a -> unit) Eliom_lib.shared_value

    val synced : 'a t -> bool

  end

end

module ReactiveData : sig
  module RList : sig
    include Eliom_shared_sigs.RLIST
      with type 'a signal := 'a React.S.t
    val synced : 'a t -> bool
  end
end
}}

{client{
module React : sig

  module S : sig

    include module type of React.S

    include Eliom_shared_sigs.S with type 'a t := 'a t

    val create :
      ?eq:('a -> 'a -> bool) ->
      ?default:('a t * (?step:React.step -> 'a -> unit)) ->
      ?reset_default:bool ->
      'a -> 'a React.signal * (?step:React.step -> 'a -> unit)

  end

end

module ReactiveData : sig

  module RList : sig

    include module type of ReactiveData.RList
    with type 'a t = 'a ReactiveData.RList.t
     and type 'a handle = 'a ReactiveData.RList.handle

    include Eliom_shared_sigs.RLIST
      with type 'a t := 'a t
       and type 'a handle := 'a handle
       and type 'a signal := 'a React.S.t

    val make :
      ?default:('a t Eliom_lib.client_value *
                'a handle Eliom_lib.client_value) ->
      ?reset_default:bool ->
      'a list ->
      'a t * 'a handle

  end

end
}}
