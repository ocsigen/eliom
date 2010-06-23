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


(* It does not seem possible to write anything in camlp4 w/out opening that *)
open Camlp4
open Camlp4.PreCast


(*** Options for parser ***)
let client_file = ref "client_file.ml"
let _ = Camlp4.Options.add
          "-client"
          (Arg.Set_string client_file)
          "set client code output file name"
let pretty = ref false
let _ = Camlp4.Options.add
          "-pretty"
          (Arg.Set pretty)
          "pretty print client code instead of dumping it"
(*TODO: option for plain text output DumpOCamlAst <- OCaml*)


(*** Syntax Identity module ***)
module Id : Camlp4.Sig.Id = struct

  let name = "Eliom/Client-Server Symmetric Syntax"
  let version = "building"

end

(*** Core ***)
module Make (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  include Syntax


  (*TODO: extensible wrapper list *)
  let wrappers =
    let _loc = Loc.ghost in
    [
      ("w",
       (<:expr<Eliom_client.wrap ~sp>>, <:expr<Eliom_obrowser.unwrap>>));
      ("sp",
       (<:expr<Eliom_client.wrap_sp>>, <:expr<Eliom_obrowser.unwrap_sp>>));
      ("node",
       (<:expr<Eliom_client.wrap_node ~sp>>, <:expr<Eliom_obrowser.unwrap_node>>));
      ("channel",
       (<:expr<Eliom_comet.wrap ~sp>>, <:expr<Eliom_client_comet.unwrap>>));
      ("up_event",
       (<:expr<Eliom_event.Up.wrap>>, <:expr<Eliom_client_event.Up.unwrap ~sp>>));
      ("down_event",
       (<:expr<Eliom_event.Down.wrap ~sp>>, <:expr<Eliom_client_event.Down.unwrap>>));
    ]
  exception Not_a_registered_wrapper of string

  let find_wrapper i =
    try
      (List.assoc i wrappers)
    with
      | Not_found -> raise (Not_a_registered_wrapper i)



  let rnd = Random.State.make [|0x513511d4|]
  let random_int64 () = Random.State.int64 rnd 0x100000000L
  let random_arg_name () =
    Format.sprintf "a%08Lx" (random_int64 ())


  (* Client printer *)
  module Client_code_dumper = Camlp4.Printers.DumpOCamlAst.Make(Syntax)
  module Client_code_printer = Camlp4.Printers.OCaml.Make(Syntax)

  let client_dump = (* from pa_eliom_obrowser, KISSer way ? *)
    ref (let _loc = Loc.ghost in <:str_item< (* generated file *) >>)
  let emit_client _loc ss =
    List.iter
      (fun s -> client_dump := <:str_item< $!client_dump$ ;; $s$ ;; >>)
      ss
  let _ =
    at_exit
      (fun () ->
         (if !pretty
          then Client_code_printer.print_implem
          else Client_code_dumper.print_implem
         )
           ~output_file:(!client_file)
           !client_dump
      )


    

  let register_closure _loc num args expr =
    let rec clo_args_aux acc = function
      | [] -> <:patt< ($acc$) >>
      | n::tl ->
         clo_args_aux <:patt< $acc$,$lid:n$ >> tl
    in
    let clo_arg = match args with
      | [] -> <:patt< () >>
      | n::ns as l -> clo_args_aux <:patt< $lid:n$ >> ns
    in
    <:str_item<
      let _ = Eliom_obrowser.register_closure
                $`int:Int64.to_int num$
                (fun $clo_arg$ -> $expr$)
    >>

    

  let closure_call _loc num args =
    let rec expr_of_args_aux acc = function
      | [] -> <:expr< ($acc$) >>
      | e::tl ->
         expr_of_args_aux <:expr< $acc$,$e$ >> tl
    in
    let expr_of_args = match args with
      | [] ->  (* placing 'unit' when no arguments are used *)
          <:expr< () >>
      | [e] -> e
      | _::_::_ as l -> expr_of_args_aux <:expr< >> l
    in
    <:expr<
         $str:"caml_run_from_table(" ^ string_of_int (Int64.to_int num) ^ ","$
       ^ Eliom_client_types.jsmarshal $expr_of_args$ ^ ")"
    >>


(*
  (* Adding a few keywords *)

  (* defining a filter to add '{{', '{...{' and '}}' as keywords *)
  let client_server_filter old_filter stream =
    let rec f = parser
      | [< '(Sig.KEYWORD "{", loc) ; whatnot >] ->
          (match whatnot with parser
             | [< '(Sig.KEYWORD "{", _) ; wwhatnot >] ->
                 (match wwhatnot with parser
                    | [< '(Sig.LIDENT "client", _) ; wwwhatnot >]
                     -> [< '(Sig.KEYWORD "{{client", loc) ; f wwwhatnot >]
                    | [< '(Sig.LIDENT "shared", _) ; wwwhatnot >]
                     -> [< '(Sig.KEYWORD "{{shared", loc) ; f wwwhatnot >]
                    | [< '(Sig.LIDENT "server", _) ; wwwhatnot >]
                     -> [< '(Sig.KEYWORD "{{server", loc) ; f wwwhatnot >]
                 )
          )
      | [< '(Sig.KEYWORD "}", loc) ; whatnot >] ->
          (match whatnot with parser
             | [< '(Sig.KEYWORD "}", _) ; wwhatnot >]
                     -> [< '(Sig.KEYWORD "}}", loc) ; f wwhatnot >]
             | [< wwhatnot>]
                     -> [< '(Sig.KEYWORD "}", loc) ; f wwhatnot >]
          )
      | [< 'smthg ; whatnot >] -> [< 'smthg ; f whatnot >]
    in
      old_filter (f stream)

  (* registering filter *)
  let () = Token.Filter.define_filter
             (Gram.get_filter ())
             client_server_filter
*)


  (* Parsing with side effects... Ugly but it just works ! *)
  let client_exprs_ref = ref []
  let inside = ref false

  (* Parsing exceptions *)
  exception Dollar_bounded_expr_outside_curly_bounds
  exception Nested_curly_bounds

  (* Extending syntax *)

  EXTEND Gram
    GLOBAL: str_item expr;

    str_item : BEFORE "top"
               [[ "{" ; "shared" ; "{" ; s = empty_start ;
                    e = LIST0 SELF ;

                  "}" ; "}" ; s = empty_stop ->
                    (emit_client _loc e ;
                     List.fold_left
                       (fun x y -> <:str_item< $x$ ;; $y$ ;; >>)
                       <:str_item< >>
                       e
                    )
                | "{" ; "server" ; "{" ; e = LIST0 SELF ; "}" ; "}" ->
                     (List.fold_left
                        (fun x y -> <:str_item< $x$ ;; $y$ ;; >>)
                        <:str_item< >>
                        e
                     )
                | "{" ; "client" ; "{" ; s = empty_start ;
                    e = LIST0 SELF ;
                  "}" ; "}" ; ss = empty_stop ->
                    (emit_client _loc e ;
                     <:str_item< >>
                    )
               ]];

    empty_start : [[ -> if !inside = true
                        then raise Nested_curly_bounds
                        else (inside := true ; client_exprs_ref := [])
                  ]];
    empty_stop : [[ -> inside := false ]];

    expr : BEFORE "simple"
           [[ KEYWORD "{" ; KEYWORD "{" ; s = empty_start ;
                e = SELF ;
              KEYWORD "}" ; KEYWORD "}" ; ss = empty_stop ->
                (let num = random_int64 () in
                 let (cl_args, serv_args) = List.split !client_exprs_ref in
                 emit_client _loc [register_closure _loc num cl_args e] ;
                 <:expr< $closure_call _loc num serv_args$ >>
                )

            | "\\" ; i = a_LIDENT ; ":" ; e = expr ->
                if !inside = false
                then raise Dollar_bounded_expr_outside_curly_bounds
                else
                  (let n = random_arg_name () in
                   let (wrapper, unwrapper) = find_wrapper i in
                   client_exprs_ref :=
                     (n, <:expr< $wrapper$ $e$>>) :: !client_exprs_ref ;
                   <:expr< $unwrapper$ $lid:n$ >>
                  )
            ]];

  END


end

(* Registration side effect *)
let module M = Register.OCamlSyntaxExtension(Id)(Make) in ()
