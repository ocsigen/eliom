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


(* It does not seem reasonable to write some camlp4 w/out opening that *)
open Camlp4
open Camlp4.PreCast


(*** Options for parser ***)
(* There are only to options for now :
 * -client <filename>     to indicate the file in which the client code should
 *                        be printed.
 * -pretty                to set pretty printing of client code (instead of AST
 *                        dump). This is provided for syntax extension debugging
 * *)

(* The file the client side is sent to *)
let client_file = ref "client_file.ml"
let _ = Camlp4.Options.add
          "-client"
          (Arg.Set_string client_file)
          "set client code output file name"

(* option for pretty printing of client side. The default behavior is to dump
 * the client code AST. *)
let pretty = ref false
let _ = Camlp4.Options.add
          "-pretty"
          (Arg.Set pretty)
          "pretty print client code instead of dumping it"



(*** Syntax Identity module ***)
module Id : Camlp4.Sig.Id = struct

  let name = "Eliom/Client-Server Symmetric Syntax"
  let version = "alpha"

end



(*** Core ***)
module Make (Syntax : Camlp4.Sig.Camlp4Syntax) = struct
  (* Syntax inclusion seems necessary. Although a would be doc might explain
   * why, none is available. *)
  include Syntax


  (*TODO: extensible wrapper list *)
  (* This is the list associating wrapper-keywords to actual wrappers. *)
  let wrappers =
    let _loc = Loc.ghost in
    [
      ("w",
       (<:expr<Eliommod_cli.wrap ~sp>>, <:expr<Eliommod_cli.unwrap>>));
      ("sp",
       (<:expr<Eliommod_cli.wrap_sp>>, <:expr<Eliommod_cli.unwrap_sp>>));
      ("node",
       (<:expr<Eliommod_cli.wrap_node ~sp>>, <:expr<Eliommod_cli.unwrap_node>>));
      ("channel",
       (<:expr<Eliom_comet.Channels.wrap ~sp>>, <:expr<Eliom_client_comet.Channels.unwrap>>));
      ("buffchan",
       (<:expr<Eliom_comet.Dlisted_channels.wrap ~sp>>, <:expr<Eliom_client_comet.Dlisted_channels.unwrap>>));
      ("up_event",
       (<:expr<Eliom_event.Up.wrap>>, <:expr<Eliom_client_event.Up.unwrap ~sp>>));
      ("down_event",
       (<:expr<Eliom_event.Down.wrap ~sp>>, <:expr<Eliom_client_event.Down.unwrap>>));
    ]
  (* Exception raised when an unknown wrapper-keyword is used. *)
  exception Not_a_registered_wrapper of string

  (* associating wrapper-keywords *)
  let find_wrapper i =
    try (List.assoc i wrappers)
    with Not_found -> raise (Not_a_registered_wrapper i)



  (* Random int64 and name generation. Collisions are possible but unlikely. *)
  let rnd = Random.State.make [|0x513511d4|]
  let random_int64 () = Random.State.int64 rnd 0x100000000L
  let random_arg_name () =
    Format.sprintf "a%08Lx" (random_int64 ())


  (* Client printers *)
  module Client_code_dumper = Camlp4.Printers.DumpOCamlAst.Make(Syntax)
  module Client_code_printer = Camlp4.Printers.OCaml.Make(Syntax)

  (* A reference to an empty str_item *)
  let client_dump = (* from pa_eliom_obrowser, KISSer way ? *)
    ref (let _loc = Loc.ghost in <:str_item< (* generated file *) >>)
  (* The reference is updated for each piece of client code. *)
  let emit_client _loc ss =
    List.iter
      (fun s -> client_dump := <:str_item< $!client_dump$ ;; $s$ ;; >>)
      ss
  (* The client file is written once *)
  let _ =
    at_exit
      (fun () ->
         (if !pretty (* Depending on !pretty, a different printer is used *)
          then Client_code_printer.print_implem
          else Client_code_dumper.print_implem
         )
           ~output_file:(!client_file)
           !client_dump
      )


    

  (* Client side code emission. *)
  let register_closure _loc num args expr =
    let rec clo_args_aux acc = function
      | [] -> <:patt< ($acc$) >>
      | n::tl ->
         clo_args_aux <:patt< $acc$,$lid:n$ >> tl
    in
    let clo_arg = match args with
      | [] -> <:patt< () >>
      | n::ns -> clo_args_aux <:patt< $lid:n$ >> ns
    in
    <:str_item<
      let _ = Eliommod_cli.register_closure
                $`int:Int64.to_int num$
                (fun $clo_arg$ -> $expr$)
    >>



  (* Server side code emission *)
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



  (* We use parsing with side effects... It's ugly but it works ! *)
  let client_exprs_ref = ref [] (* This ref accumulates \wrapper:expr while in a
                                   {{ expr }} *)
  let inside = ref false (* true when inside a {{ expr }} ; false when not. *)

  (* Parsing exceptions *)
  exception Wrapper_expr_outside_curly_bounds
  exception Nested_curly_bounds

  (* Extending syntax *)

  EXTEND Gram
    GLOBAL: str_item expr;

    (* dummy rules for side effects *)
    empty_start : [[ -> if !inside = true
                        then raise Nested_curly_bounds
                        else (inside := true ; client_exprs_ref := [])
                  ]];
    empty_stop : [[ -> inside := false ]];

    (* To str_item we add {client{ LIST0 SELF }}; {server{ LIST0 SELF }}; and
     * {shared{ LIST0 SELF }} *)
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

    (* To expr we add {{ SELF }} and \lid:SELF *)
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
                then raise Wrapper_expr_outside_curly_bounds
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
