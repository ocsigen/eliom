open Camlp4.PreCast;;
module CamlSyntax = Camlp4OCamlParser.Make(Camlp4OCamlRevisedParser.Make(Syntax));;

module ClientDump = Camlp4.Printers.OCaml.Make (CamlSyntax) ;;
 
let expr_of_string = CamlSyntax.Gram.parse_string CamlSyntax.expr_eoi;;
 
module ObroGram = Gram;;
 
let obrofun = ObroGram.Entry.mk "O'Browser function";;
let obrofun_eoi = ObroGram.Entry.mk "O'Browser function quotation";;
let client_eoi = ObroGram.Entry.mk "O'Browser client code quotation";;

let hash loc =
  let combine h v = h := !h + v (* ahem... *) in
  let h = ref 0 in
    combine h (Loc.start_pos loc).Lexing.pos_cnum ;
    let fn = Loc.file_name loc in
      for i = 0 to String.length fn - 1 do
	combine h (Char.code fn.[i] lsl (8 * (i mod 4)))
      done ; !h land 0x7FFFFFFF
;;

Camlp4_config.antiquotations := true;;

let tup_args args =
  let rec tup_args = function
    | [] -> assert false
    | (p, t, l) :: [] -> (p, t, l)
    | (p, t, l) :: tl ->
	let ep, et, el = tup_args tl in
	let l' = Loc.merge l el in
	  (CamlSyntax.Ast.PaCom (l', p, ep), CamlSyntax.Ast.TySta (l', t, et), l')
  in 
    match args with
      | [ p, t, _ ] -> p, t
      | _ ->
	  let p, t, l = tup_args args in
	    (CamlSyntax.Ast.PaTup (l, p), CamlSyntax.Ast.TyTup (l, t))
;;

let server_obrofun args loc =
  let mkarg =
    let uniq = ref 0 in
      fun () -> incr uniq ; Printf.sprintf "arg%d" !uniq
  in
  let rec tup_args n l =
    match l with
      | [] ->
	  begin match n with
	    | None -> assert false
	    | Some (a, l') ->
		let e = CamlSyntax.Ast.ExTup (l', a) in
		let _loc = loc in
		let s = Printf.sprintf "caml_run_from_table (main_vm, 0x%X," (hash loc) in
		  <:expr< $str:s$ ^ Eliom_obrowser.jsmarshal $e$ ^ ")" >>
	  end
      | (_, t, l) :: tl ->
	  let na = mkarg () in
	  let e = tup_args
	    (Some (let _loc = l in
		     begin match n with
		       | None ->
			   <:expr< $lid:na$ >>, _loc
		       | Some (a, l') ->
			   let l'' = Loc.merge l l' in
			     CamlSyntax.Ast.ExCom (l'', a, <:expr< $lid:na$ >>), l''
		     end)) tl
	  in let _loc = loc in <:expr< fun ( $lid:na$ : $t$) -> $e$ >>
  in tup_args None args
;;

let client_file = ref "_client.ml"

let client_str = ref (let _loc = Loc.ghost in <:str_item< (* generated file *) >>)

let set_prologue m =
  let old = !client_str in
    client_str := (let _loc = Loc.ghost in <:str_item< $old$ ;; include $uid:m$ >>)
;;

let client_obrofun args loc e =
  let _loc = loc in
  let id = Printf.sprintf "0x%X" (hash loc) in
  let old = !client_str in
  let p, t = tup_args args in
  let e = <:expr< fun ($p$ : $t$) -> $e$ >> in
    client_str := <:str_item< $old$ ;; let _ = Eliom_obrowser_client.register_closure $int:id$ ($e$) >> ;
    ClientDump.print_implem
      ~output_file:(!client_file)
      !client_str
;;

let dump_obrofun args loc e =
  client_obrofun args loc e ;
  server_obrofun args loc
;;

let dump_client loc e =
  let _loc = loc in
  let old = !client_str in
  client_str := <:str_item< $old$ ;; $e$ >> ;
  ClientDump.print_implem
    ~output_file:(!client_file)
    !client_str;
  <:str_item<>>
;;


EXTEND ObroGram
  GLOBAL: obrofun obrofun_eoi client_eoi ;
  obrofun: [
    "without_args"
      [ "(" ; ")" ; "->" ; e = CamlSyntax.expr -> dump_obrofun [(<:patt< () >>, <:ctyp< unit >>, _loc)] _loc e ]
  | "with_args"
      [ l = args ; "->" ; e = CamlSyntax.expr -> dump_obrofun l _loc e ]
    ];
  args: [
    "arg" RIGHTA
      [ "(" ; p = CamlSyntax.patt ; ":" ; t = CamlSyntax.ctyp ; ")" ; r = args -> (p, t, _loc) :: r ]
  | "end"
      [ -> [] ]
  ];
  obrofun_eoi: [
    [ f = obrofun -> f ]
  ];
  client_eoi: [
    [ phrase = CamlSyntax.str_items ; `EOI -> dump_client _loc phrase ]
  ];
 END;;
 
let expand_obrofun_quot_expr loc _loc_name_opt quotation_contents =
  ObroGram.parse_string obrofun_eoi loc quotation_contents;;

let print_client_code loc _loc_name_opt quotation_contents =
  ObroGram.parse_string client_eoi loc quotation_contents;;

Syntax.Quotation.add "obrofun" Syntax.Quotation.DynAst.expr_tag expand_obrofun_quot_expr;;

Syntax.Quotation.add "obrowser" Syntax.Quotation.DynAst.str_item_tag print_client_code;;




Camlp4.Options.add "-client" (Arg.Set_string client_file) "set client code output file name" ;;
Camlp4.Options.add "-prologue" (Arg.String set_prologue) "set client handwritten prologue module" ;;

(* Syntax.Quotation.default := "obrofun";; *)
