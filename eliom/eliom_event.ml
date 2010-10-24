(* Ocsigen
 * http://www.ocsigen.org
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

(* Module for event wrapping and related functions *)


include Eliommod_event

module Up =
struct

  type 'a t =
      'a React.event *
      (unit,
       'a,
       [ `Nonattached of [ `Post ] Eliom_services.na_s ],
       [ `WithoutSuffix ],
       unit,
       [ `One of 'a Eliom_parameters.caml ] Eliom_parameters.param_name,
       [ `Registrable ],
       Eliom_predefmod.Action.return)
        Eliom_services.service

  let to_react = fst
  let wrap ~sp (_, s) = Eliommod_cli.wrap ~sp s

  (* An event is created along with a service responsible for it's occurences.
   * function takes sp and a param_type *)
  let create ?sp ?scope ?name post_params =
    let (e, push) = React.E.create () in
    let scope = match sp, scope with
      | _, Some l -> l
      | None, _ -> `Global
      | _ -> `Client_process
    in
    let e_writer = Eliom_services.post_coservice' ?name ~post_params () in
    Eliom_predefmod.Action.register
      ~scope
      ~options:`NoReload
      ?sp
      ~service:e_writer
      (fun _ () value -> push value ; Lwt.return ());
    (e, e_writer)

end
