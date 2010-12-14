(*pp $CAMLP4OF *)

(* Copyright Grégoire Henry 2010.
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
  let classname = "Json"
  let runtimename = "Deriving_Json"
  let predefs = [
    ["int"      ], "int";
    ["bool"     ], "bool";
    ["unit"     ], "unit";
    ["char"     ], "char";
    ["int32"    ], "int32";
    ["Int32";"t"], "int32";
    ["int64"    ], "int64";
    ["Int64";"t"], "int64";
    ["nativeint"], "nativeint";
    ["float"    ], "float";
    (* ["num"], "num"; *)
    ["string"   ], "string";
    ["list"     ], "list";
    ["ref"      ], "ref";
    ["option"   ], "option";
    ["array"    ], "array";
  ]
  let default_module = Some ("Defaults'","min'")
  let allow_private = false
  let depends = []
end

module Make(H : DeriverHelpers) : Deriver = struct

  open H

  let wrap
      ?(read_variant = [ <:match_case< _ -> assert false >> ])
      ?(hashes = <:expr< assert false >>)
      ~write ~read () =
    [ <:str_item< let write buffer = function $list:write$ >>;
      <:str_item< let match_variant hash = $hashes$ >>;
      <:str_item<
	let read_variant buf hash = match hash with $list:read_variant$	>>;
      <:str_item< let read buf = $read$ >> ]

  let instance = object (self)

    inherit generator as super

    (* Generate code that write a block with [tag].*)
    method do_dump_blk ctxt tag contents =
      let args_dumpers = List.map
	  (fun (var, ty) ->
	    <:expr<
	      Buffer.add_string buffer ",";
	      $self#call_poly_expr ctxt ty "write"$ buffer $lid:var$ >>)
	   contents in
      <:expr<
        Buffer.add_string buffer $str:"["^string_of_int tag$;
        $seq_list args_dumpers$;
        Buffer.add_string buffer "]"
      >>

    method tuple ctxt tys =
      let size = List.length tys in
      let vars, patt, expr = tuple size in
      let contents = List.map2 (fun var ty -> (var, ([], ty))) vars tys in
      let dumper =
	<:match_case< $patt$ -> $self#do_dump_blk ctxt 0 contents$ >> in
      let readers =
	List.fold_right2
	  (fun var ty expr -> <:expr<
	    Deriving_Json_lexer.read_comma buf;
	    let $lid:var$ = $self#call_expr ctxt ty "read"$ buf in $expr$ >>)
	  vars tys
	  <:expr<
	    Deriving_Json_lexer.read_rbracket buf;
            $expr$ >> in
      let read = <:expr<
	Deriving_Json_lexer.read_lbracket buf;
        ignore(Deriving_Json_lexer.read_bounded_int buf ~min:0 ~max:0);
	$readers$ >> in
      wrap ~write:[dumper] ~read ()

    method case ctxt (cst_tag, ncst_tag, dumpers, readers) (ctor, tys) =
      match tys with
      | [] ->
	  let dumper = <:match_case< $uid:ctor$ ->
	    Buffer.add_string buffer $str:string_of_int cst_tag$ >> in
	  let reader = <:match_case< `Cst $int:string_of_int cst_tag$ -> $uid:ctor$ >> in
	  (succ cst_tag, ncst_tag, dumper::dumpers, reader::readers)
      | tys ->
	  let size = List.length tys in
	  let vars, patt, expr = tuple size in
	  let contents = List.map2 (fun var ty -> (var, ([], ty))) vars tys in
	  let dumper =
	    <:match_case< $uid:ctor$ $patt$ ->
	      $self#do_dump_blk ctxt ncst_tag contents$ >> in
	  let reader =
	    List.fold_right2
	      (fun var ty expr -> <:expr<
		Deriving_Json_lexer.read_comma buf;
		let $lid:var$ = $self#call_expr ctxt ty "read"$ buf in $expr$ >>)
	      vars tys
	      <:expr<
	        Deriving_Json_lexer.read_rbracket buf;
	        $uid:ctor$ $expr$ >> in
	  let reader =
	    <:match_case< `NCst $int:string_of_int ncst_tag$ -> $reader$ >> in
	  (cst_tag, succ ncst_tag, dumper::dumpers, reader::readers)

    method sum ?eq ctxt decl subst summands =
      let msg = Printf.sprintf "Json_%s: Unexpected constructor." classname in
      let failover = <:match_case< _ -> failwith $str:msg$ >> in
      let _, _, dumpers, readers =
	List.fold_left (self#case ctxt) (0,0,[],[failover]) summands in
      let read = <:expr<
	match Deriving_Json_lexer.read_case buf with
	$list:readers$ >> in
      wrap ~write:dumpers ~read ()

    method record ?eq ctxt decl subst fields =
      if List.exists (fun (_, _, mut) -> mut = `Mutable) fields then
	failwith "Can't derive Json serializer for mutable records.";
      let patt = record_pattern fields in
      let contents = List.map (fun (name, ty, _) -> name, ty) fields in
      let dumper =
	<:match_case< $patt$ -> $self#do_dump_blk ctxt 0 contents$ >> in
      let readers =
	List.fold_right
	  (fun (var, ty, _) expr ->
	    <:expr<
	      Deriving_Json_lexer.read_comma buf;
	      let $lid:var$ = $self#call_poly_expr ctxt ty "read"$ buf in $expr$ >>)
	  fields
	  <:expr<
            Deriving_Json_lexer.read_rbracket buf;
	    $record_expression fields$ >> in
      let read = <:expr<
	Deriving_Json_lexer.read_lbracket buf;
        ignore(Deriving_Json_lexer.read_bounded_int buf ~min:0 ~max:0);
	$readers$ >> in
      wrap ~write:[dumper] ~read ()

    method polycase ctxt tagspec =
      match tagspec with
      | Tag (name, []) ->
	  let hash = Utils.tag_hash name in
	  <:match_case< `$uid:name$ ->
	    Buffer.add_string buffer $str:string_of_int hash$ >>,
	  <:match_case< `Cst $int:string_of_int hash$ -> `$name$ >>,
	  <:expr< hash = `Cst $int:string_of_int hash$ >>
      | Tag (name, [ty]) ->
	  let hash = Utils.tag_hash name in
	  let contents = ["tag", ([],`Constr(["int"],[])) ; "x", ([],ty) ] in
	  <:match_case< `$uid:name$ x ->
	    let tag = $int:string_of_int hash$ in
	    $self#do_dump_blk ctxt 0 contents$ >>,
	  <:match_case< `NCst $int:string_of_int hash$ ->
	    Deriving_Json_lexer.read_comma buf;
	    let c = $self#call_expr ctxt ty "read"$ buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    `$name$ c >>,
	    <:expr< hash = `NCst $int:string_of_int hash$ >>
      | Tag (name, tys) ->
	  let hash = Utils.tag_hash name in
	  let ty = `Tuple tys in
	  let contents = ["tag", ([],`Constr(["int"],[])) ; "x", ([],ty) ] in
	  <:match_case< `$uid:name$ x ->
	    let tag = $int:string_of_int hash$ in
	    $self#do_dump_blk ctxt 0 contents$ >>,
	  <:match_case< `NCst $int:string_of_int hash$ ->
	    Deriving_Json_lexer.read_comma buf;
	    let c = $self#call_expr ctxt ty "read"$ buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    `$name$ c >>,
	    <:expr< hash = `NCst $int:string_of_int hash$ >>
      | Extends t ->
          let patt, guard, cast = cast_pattern ctxt t in
          <:match_case< $patt$ when $guard$ ->
            $self#call_expr ctxt t "write"$ buffer $cast$ >>,
          <:match_case< hash when $self#call_expr ctxt t "match_variant"$ hash ->
            ($self#call_expr ctxt t "read_variant"$ buf hash :> a) >>,
          <:expr< $self#call_expr ctxt t "match_variant"$ hash >>

    method variant ctxt decl subst (_,tags) =
      let msg = Printf.sprintf "Json_%s: Unexpected constructor." classname in
      let failover = <:match_case< _ -> failwith $str:msg$ >> in
      let dumpers, readers, hashes = List.split3 (List.map (self#polycase ctxt) tags) in
      let read = <:expr< read_variant buf (Deriving_Json_lexer.read_vcase buf) >> in
      let hashes =
	List.fold_right
	  (fun e1 e2 -> <:expr< $e1$ || $e2$ >>)
	  hashes <:expr< false >> in
      wrap
	~read_variant:(readers @ [failover]) ~hashes
	~write:(dumpers@[failover]) ~read ()

    method alpha ctxt ty =
      wrap
	~write:[ <:match_case< _ -> assert false >> ]
	~read:<:expr< assert false >> ()

  end

  let generate = instance#rhs

end

let _ = Base.register (module Params: DeriverParams) (module Make : MakeDeriver)
