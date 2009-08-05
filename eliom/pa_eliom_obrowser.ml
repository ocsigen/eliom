open Camlp4

module Id : Sig.Id = struct
  let name = "Eliom/O'Browser"
  let version = "0"
end

module Make (Syntax : Sig.Camlp4Syntax) = struct
  open Sig
  include Syntax

  let obrofun = Gram.Entry.mk "O'Browser function"
  let obrofun_eoi = Gram.Entry.mk "O'Browser function quotation"

  let hash loc =
    let combine h v = h := !h + v (* ahem... *) in
    let h = ref 0 in
      combine h (Loc.start_pos loc).Lexing.pos_cnum ;
      let fn = Loc.file_name loc in
	for i = 0 to String.length fn - 1 do
	  combine h (Char.code fn.[i] lsl (8 * (i mod 4)))
	done ; !h land 0x7F_FF_FF_FF

  let tup_args args =
    let rec tup_args = function
      | [] -> assert false
      | (p, t, l) :: [] -> (p, t, l)
      | (p, t, l) :: tl ->
	  let ep, et, el = tup_args tl in
	  let l' = Loc.merge l el in
	    (Ast.PaCom (l', p, ep), Ast.TySta (l', t, et), l')
    in 
      match args with
	| [ p, t, _ ] -> p, t
	| _ ->
	    let p, t, l = tup_args args in
	      (Ast.PaTup (l, p), Ast.TyTup (l, t))
  
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
		  let e = Ast.ExTup (l', a) in
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
			       Ast.ExCom (l'', a, <:expr< $lid:na$ >>), l''
		       end)) tl
	    in let _loc = loc in <:expr< fun ( $lid:na$ : $t$) -> $e$ >>
    in tup_args None args

  let client_file = ref "_client.ml"
    
  let client_str = ref (let _loc = Loc.ghost in <:str_item< (* generated file *) >>)
    
  let set_prologue m =
    let old = !client_str in
      client_str := (let _loc = Loc.ghost in <:str_item< $old$ ;; open $uid:m$ >>)

  module ClientDump = Camlp4.Printers.OCaml.Make (Syntax) ;;
	
  let client_obrofun args loc e =
    let _loc = loc in
    let id = Printf.sprintf "0x%X" (hash loc) in
    let old = !client_str in
    let p, t = tup_args args in
    let e = <:expr< fun ($p$ : $t$) -> $e$ >> in
    client_str := <:str_item< $old$ ;; let _ = Eliom_obrowser_client.register_closure $int:id$ ($e$) ;; >> ;
    ClientDump.print_implem
      ~output_file:(!client_file)
      !client_str
  let dump_obrofun args loc e =
    client_obrofun args loc e ;
    server_obrofun args loc

  let dump_obroglo loc s =
    let _loc = loc in
    let old = !client_str in
    client_str := <:str_item< $old$ ;; $s$ ;; >> ;
    ClientDump.print_implem
      ~output_file:(!client_file)
      !client_str
  
  EXTEND Gram
	GLOBAL: obrofun obrofun_eoi fun_def str_item;
      obrofun: [
	"without_args"
	  [ "(" ; ")" ; "->" ; e = expr -> dump_obrofun [(<:patt< () >>, <:ctyp< unit >>, _loc)] _loc e ]
      | "with_args"
	  [ l = args ; "->" ; e = expr -> dump_obrofun l _loc e ]
      ];
      str_item: [
	[ "begin" ; "." ; "client" ; s = str_items ; "end" -> dump_obroglo _loc s ; <:str_item< (* ---8<-- client code --->8-- *) >>]
      ];
      args: [
	"arg" RIGHTA
	  [ "(" ; p = patt ; ":" ; t = ctyp ; ")" ; r = args -> (p, t, _loc) :: r ]
      | "end"
	  [ -> [] ]
      ];
      fun_def: [
	[ "." ; "client" ; f = obrofun -> f ]
      ];
      obrofun_eoi: [
	[ f = obrofun ; `EOI -> f ]
      ];
  END
    
  let _ = Camlp4.Options.add "-client" (Arg.Set_string client_file) "set client code output file name" ;;
  let _ = Camlp4.Options.add "-prologue" (Arg.String set_prologue) "set client handwritten prologue module" ;;
end

let module M = Register.OCamlSyntaxExtension (Id) (Make) in ()
