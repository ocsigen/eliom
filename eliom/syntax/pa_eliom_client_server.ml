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

  let name = "Eliom/Client-Server Symmetric Syntax (server part)"
  let version = "alpha"

end



(*** OPTIONS ***)
let type_file = ref "syntax_temp_files/server_type_inference.mli"
let _ = Camlp4.Options.add "-typefile" (Arg.Set_string type_file)
          "type inferrence file"



(*** FILTER: server ***)
module Make_server_filter (Filters : Camlp4.Sig.AstFilters) = struct
  module Reader = Pa_eliom_seed.Make_reader (Filters)
  module Common = Pa_eliom_seed.Pre_make_filter(Filters)

  include Common

  let inferred_sig = lazy (Reader.load_file !type_file)
  let find_val_type n =
    match Reader.find_value_type n (Lazy.force inferred_sig) with
      | Some t -> Reader.strip_option_ref t
      | None -> failwith "Type not inferred"

  (* Server side code emission *)
  let closure_call _loc num args =
    <:expr<
         $str:"caml_run_from_table("
          ^ Int64.to_string num
          ^ ", \'" $
       ^ Eliom_client_types.jsmarshal $expr_nested_tuple_of_expr_list _loc args$
       ^ "\')"
    >>

  class eliom_cli_serv_serv_emiter_map =
  object (self)
    inherit eliom_cli_serv_map as super

    val mutable arg_collection = []
    method push_arg _loc e n =
      let typ = find_val_type n in
      let wrapper = (*common*)find_wrapper _loc typ in
      arg_collection <- <:expr< ($wrapper$ $e$) >> :: arg_collection;
      <:expr< >>
    method flush_args =
      let res = arg_collection in
      arg_collection <- [];
      res

    method server_str_item _loc s = <:str_item< $s$ >>
    method client_str_item _loc s = failwith "Server file has client code"
    method client_expr _loc e i = closure_call _loc i self#flush_args
    method server_escaped_expr _loc e n = self#push_arg _loc e n

  end

  let mapper = new eliom_cli_serv_serv_emiter_map
  let filter s = mapper#str_item s

  let () =
    (*Filters.*)register_str_item_filter
      (fun si ->
         let _loc = Ast.loc_of_str_item si in
         filter si
      )


end

open Pa_eliom_seed
let () = match get_pass () with
  | Server_pass ->
      let module M = Camlp4.Register.AstFilter(Id)(Make_server_filter) in ()
  | Client_pass | Type_pass | Raw_pass -> ()
