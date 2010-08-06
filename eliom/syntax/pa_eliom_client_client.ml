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


(*** Syntax Identity module ***)
module Id : Camlp4.Sig.Id = struct

  let name = "Eliom/Client-Server Symmetric Syntax (client part)"
  let version = "alpha"

end



(*** OPTIONS ***)
let type_file = ref "syntax_temp_files/server_type_inference.mli"
let _ =
  Camlp4.Options.add
    "-typefile"
    (Arg.Set_string type_file)
    "type inferrence file"



(*** FILTER: client ***)
module Make_client_filter (Filters : Camlp4.Sig.AstFilters) = struct
  module Reader = Pa_eliom_seed.Make_reader (Filters)
  module Common = Pa_eliom_seed.Pre_make_filter (Filters)

  include Common

  let inferred_sig = lazy (Reader.load_file !type_file)
  let find_val_type n =
    match Reader.find_value_type n (Lazy.force inferred_sig) with
      | Some t -> Reader.strip_option_ref t
      | None -> failwith "Type not inferred"


  (* Client side code emission. *)
  let register_closure _loc num args expr =
    <:expr<
      Eliommod_cli.register_closure
         (Int64.to_int $`int64:num$)
         (fun $patt_nested_tuple_of_patt_list _loc args$ -> $expr$)
    >>


  class eliom_cli_serv_cli_emiter_map =
  object (self)
    inherit eliom_cli_serv_map as super

    val mutable arg_collection = ([] : Ast.patt list)
    method push_arg _loc e n =
      let typ = find_val_type n in
      arg_collection <- <:patt< $id:n$ >> :: arg_collection;
      let unwrapper = find_unwrapper _loc typ in
      <:expr< ($unwrapper$ $id:n$) >>
    method flush_args =
      let res = arg_collection in
      arg_collection <- [];
      res

    val mutable clos_collection = []
    method push_closure_registration _loc e i =
      clos_collection <-
        <:str_item< let _ = $ register_closure _loc i self#flush_args e $ >>
        :: clos_collection
    method flush_closure_registrations =
      let res = clos_collection in
      clos_collection <- [];
      res

(*
    val mutable str_collection = []
    method push_client_str_item s = 
      str_collection <- s :: str_collection
    method flush_str_items =
      let res = str_collection in
      str_collection <- [];
      res
 *)

    method server_str_item _loc s = <:str_item< >>
    method client_str_item _loc s = s
    method client_expr _loc e i =
      self#push_closure_registration _loc e i;
      <:expr< "" >>
    method server_escaped_expr _loc e n = self#push_arg _loc e n

  end

  let mapper = new eliom_cli_serv_cli_emiter_map
  let filter s = mapper#str_item s

  let () =
    (*Filters.*)register_str_item_filter
      (fun si ->
         let _loc = Ast.loc_of_str_item si in
         let new_si =
           <:str_item< 
             $list: List.map filter (Ast.list_of_str_item si [])$
           >>
         in
           <:str_item<
             $new_si$
             $list: mapper#flush_closure_registrations$ ;;
         >> 
      )

end

open Pa_eliom_seed
let () = match Pa_eliom_seed.get_pass () with
  | Client_pass ->
      let module M = Camlp4.Register.AstFilter(Id)(Make_client_filter) in ()
  | Server_pass | Type_pass | Raw_pass -> ()
