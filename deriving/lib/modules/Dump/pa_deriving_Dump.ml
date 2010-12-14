(*pp $CAMLP4OF *)

(* Copyright Jeremy Yallop 2007.
   Copyright Grégoire Henry 2010.
   This file is free software, distributed under the MIT license.
   See the file COPYING for details.
*)

open Camlp4.PreCast
open Pa_deriving_common

open Context
open Base
open Type
open Utils

module Params = struct
  let classname = "Dump"
  let runtimename = "Deriving_Dump"

  let predefs = [
    ["unit"], "unit";
    ["bool"], "bool";
    ["char"], "char";
    ["int"], "int";
    ["int32"], "int32";
    ["Int32";"t"], "int32";
    ["int64"], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"], "float";
    (* ["num"], "num"; *)
    ["string"], "string";
    ["list"], "list";
    ["option"], "option";
  ]
  let default_module = Some ("Defaults", "min")
  let allow_private = false
  let depends = []
end

module Make(H : DeriverHelpers) : Deriver = struct

  open H

  let wrap ?(buffer="buffer") ?(stream="stream") to_buffer from_stream =
    [ <:str_item< let to_buffer $lid:buffer$ = function $list:to_buffer$ >> ;
      <:str_item< let from_stream $lid:stream$ = $from_stream$ >> ]

  let dump_int ?(buffer="buffer") n =
    <:expr<
      (let module M = (val $uid:runtimename$.Dump_int.make
	                   : $uid:runtimename$.Dump with type a = int) in
       M.to_buffer) $lid:buffer$ $`int:n$ >>

  let read_int ?(stream="stream") () =
    <:expr<
      (let module M = (val $uid:runtimename$.Dump_int.make
	                   : $uid:runtimename$.Dump with type a = int) in
       M.from_stream) $lid:stream$ >>

  let instance = object (self)

    inherit generator


    method nargs ctxt vars tys : Ast.expr * Ast.expr =
      List.fold_right2
        (fun id ty (p,u) ->
          <:expr< $self#call_expr ctxt ty "to_buffer"$ buffer $lid:id$; $p$ >>,
          <:expr< let $lid:id$ = $self#call_expr ctxt ty "from_stream"$ stream in
          $u$ >>)
        vars tys
	(<:expr< >>,
         <:expr< $tuple_expr (List.map (fun id -> <:expr< $lid:id$ >>) vars)$>>)

    method tuple ctxt tys =
      let n = List.length tys in
      let vars, patt, expr = tuple n in
      let pinner, from_stream = self#nargs ctxt vars tys in
      wrap [ <:match_case< $patt$ -> $pinner$ >> ] from_stream

    method case ctxt (ctor,args) n =
      match args with
        | [] -> (<:match_case< $uid:ctor$ -> $dump_int n$ >>,
                 <:match_case< $`int:n$ -> $uid:ctor$ >>)
        | _ ->
            let nargs = List.length args in
            let vars, patt, exp = tuple nargs in
            let dump, undump = self#nargs ctxt vars args in
            <:match_case< $uid:ctor$ $patt$ ->
              $dump_int n$;
              $dump$ >>,
            <:match_case< $`int:n$ -> let $patt$ = $undump$ in $uid:ctor$ $exp$  >>

    method sum ?eq ctxt ((tname,_,_,_,_) as decl) subst summands =
      let msg =
	"Dump: unexpected tag %d at character %d when deserialising " ^ tname in
      let to_buffer, from_stream =
        List.split (List.mapn (self#case ctxt) summands) in
      let from_stream =
	<:expr< match $read_int ()$ with $list:from_stream$
                | n ->
		    raise ($uid:runtimename$.Dump_error
			     (Printf.sprintf $str:msg$ n
				(Stream.count stream))) >> in
      wrap to_buffer from_stream

    method field ctxt : Type.field -> Ast.expr * Ast.expr = function
      | (name, _, `Mutable) ->
          raise (Underivable ("Dump cannot be derived for record types "
			      ^ "with mutable fields (" ^ name ^ ")" ))
      | (name, ty, _) ->
          <:expr< $self#call_poly_expr ctxt ty "to_buffer"$ buffer $lid:name$ >>,
          <:expr< $self#call_poly_expr ctxt ty "from_stream"$ stream >>

    method record ?eq ctxt decl subst fields =
      let to_buffers, from_streams = List.split (List.map (self#field ctxt) fields) in
      let from_stream =
        List.fold_right2
          (fun (field,_,_) from_stream e ->
            <:expr< let $lid:field$ = $from_stream$ in $e$ >>)
          fields
          from_streams
          (record_expression fields) in
      let to_buffer =
	<:match_case< $record_pattern fields$ -> $seq_list to_buffers$ >> in
      wrap [to_buffer] from_stream

    method polycase ctxt tagspec n : Ast.match_case * Ast.match_case =
      match tagspec with
      | Tag (name, args) -> (match args with
        | [] ->
	    <:match_case< `$name$ -> $dump_int n$ >>,
            <:match_case< $`int:n$ -> `$name$ >>
        | tys ->
	    <:match_case< `$name$ x ->
	      $dump_int n$;
              $self#call_expr ctxt (`Tuple tys) "to_buffer"$ buffer x >>,
            <:match_case< $`int:n$ ->
              `$name$ ($self#call_expr ctxt (`Tuple tys) "from_stream"$ stream) >>)
      | Extends t ->
          let patt, guard, cast = cast_pattern ctxt t in
          <:match_case< $patt$ when $guard$ ->
            $dump_int n$; $self#call_expr ctxt t "to_buffer"$ buffer $cast$ >>,
          <:match_case< $`int:n$ ->
	    ($self#call_expr ctxt t "from_stream"$ stream :> a) >>

    method variant ctxt decl subst (_, tags) =
      let msg = "Dump: unexpected tag %d at character %d "
	        ^ "when deserialising polymorphic variant" in
      let to_buffers, from_streams =
        List.split (List.mapn (self#polycase ctxt) tags) in
      let from_stream =
	<:expr< match $read_int ()$ with $list:from_streams$
                | n -> raise ($uid:runtimename$.Dump_error
				(Printf.sprintf $str:msg$ n
                                   (Stream.count stream))) >> in
      let to_buffer = to_buffers @ [ <:match_case< _ -> assert false >>] in
      wrap to_buffer from_stream

    method alpha ctxt ty =
    [ <:str_item< let to_buffer _ _ = assert false >> ;
      <:str_item< let from_stream _ = assert false >> ]

  end

  let generate = instance#rhs

end

let _ = Base.register (module Params: DeriverParams) (module Make : MakeDeriver)
