(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010-2011
 * Raphaël Proust
 * Pierre Chambart
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

type 'a chan_id = string

external string_of_chan_id : 'a chan_id -> string = "%identity"
external chan_id_of_string : string -> 'a chan_id = "%identity"

type comet_service =
    (unit, string,
     [ `Nonattached of [ `Get | `Post ] Eliom_services.na_s ],
     [ `WithoutSuffix ], unit,
     [ `One of string ] Eliom_parameters.param_name, [ `Registrable ],
     Eliom_services.http )
      Eliom_services.service
