(* Ocsigen
 * http://www.ocsigen.org
 * Module server.ml
 * Copyright (C) 2010
 * Raphaël Proust
 * Laboratoire PPS - CNRS Université Paris Diderot
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

module Down :
sig

  val wrap :
      sp:Eliom_sessions.server_params
    -> 'a React.E.t
    -> 'a Eliom_common_comet.buffered_chan_id Eliom_client_types.data_key

end

module Up :
sig

  type 'a event

  val react_event_of_up_event : 'a event -> 'a React.E.t

  val wrap :
      sp:Eliom_sessions.server_params
    -> 'a event
    -> (unit, 'a, [ `Nonattached of [ `Post ] Eliom_services.na_s ],
        [ `WithoutSuffix ], unit, [`One of 'a] Eliom_parameters.param_name,
        [ `Registrable ], Eliom_predefmod.Action.return)
         Eliom_services.service Eliom_client_types.data_key

  val create :
       ?sp:Eliom_sessions.server_params
    -> ('a, [ `WithoutSuffix ], [`One of 'a ] Eliom_parameters.param_name)
         Eliom_parameters.params_type
    -> 'a event

end
