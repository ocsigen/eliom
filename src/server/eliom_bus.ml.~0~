(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2010
 * Raphaël Proust
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

type 'a t = {
  stream  : 'a Lwt_stream.t;
  write   : ('a -> unit);
  service : (unit,
             'a list,
             [ `Nonattached of [ `Post ] Eliom_services.na_s ],
             [ `WithoutSuffix ],
             unit,
             [ `One of 'a list Eliom_parameters.caml ]
               Eliom_parameters.param_name,
             [ `Registrable ],
             Eliom_output.Action.return
  ) Eliom_services.service;
  size : int option;
  bus_mark : 'a t Eliom_common.wrapper; (* must be the last field ! *)
}

let internal_wrap (bus: 'a t)
    : (  ('a Eliom_common_comet.chan_id)
	 * (unit,
            'a list,
            [ `Nonattached of [ `Post ] Eliom_services.na_s ],
            [ `WithoutSuffix ],
            unit,
            [ `One of 'a list Eliom_parameters.caml ] Eliom_parameters.param_name,
            [ `Registrable ],
            Eliom_output.Action.return
	 ) Eliom_services.service ) *
    Eliom_common.unwrapper
    =
  let chan = Eliom_comet.Channels.create ?size:bus.size (Lwt_stream.clone bus.stream) in
  ((Eliom_comet.Channels.get_id chan, bus.service),
   Eliom_common.make_unwrapper Eliom_common.bus_unwrap_id)

let bus_mark () = Eliom_common.make_wrapper internal_wrap

let deriving_to_list : 'a Deriving_Json.t -> 'a list Deriving_Json.t = fun (type typ) typ ->
  let (typ_list:typ list Deriving_Json.t) =
    let module M = (val Deriving_Json.Json_list.make(Deriving_Json.wrap typ):
	Deriving_Json.Json with type a = typ list) in
    M.t
  in
  typ_list

let create ?scope ?name ?size typ =
  (*The stream*)
  let (stream, push) = Lwt_stream.create () in
  let push x = push (Some x) in

  let typ_list = deriving_to_list typ in

  (*The service*)
  let post_params =
    (Eliom_parameters.caml "bus_write" typ_list
       : ('a, 'aa, 'aaa) Eliom_parameters.params_type)
  in
  let distant_write = Eliom_services.post_coservice' ?name ~post_params () in
  Eliom_output.Action.register
    ?scope
    ~options:`NoReload
    ~service:distant_write
    (fun () x -> List.iter push x ; Lwt.return ());

  (*The bus*)
  let bus =
    { stream  = stream;
      write   = push;
      service = distant_write;
      bus_mark = bus_mark ();
      size = size }
  in

  bus

let stream bus = bus.stream

let write bus x = bus.write x
