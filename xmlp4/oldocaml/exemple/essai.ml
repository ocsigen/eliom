let fff x = x
let aaa = "a"
let l = << <a $aaa$="pp">$fff aaa$</a> >>
let ff x = <:xmllist< <o>$str:x$</o><i/> >>
let l = << <a $aaa$="pp">$list:ff aaa$</a> >>

(* Pour les expressions et les patterns on peut écrire *)
let a = << a >> in
let b = "bb" in
let c = `Cc in
let d = "dd" in
let e = `Ee in
let f = "ff" in
let g = << <ark> </ark> >> in
let s = << <youpi> $a$ $str:b$ $$ $g$ <bobo $c$=$d$ $e$=$f$> </bobo> </youpi> >> in
let la = [(`A, "popo");(`Ggg, "lkjl")] in
let l = <:xmllist< <ark $c$=$f$ $list:la$> </ark> <wow> </wow> >> in
<< <youpi> $a$ zzz $list:l$ </youpi> >>
(* $$ permet d'écrire un $ *)

function << <html $list:l1$> $a$ ljl $list:l2$ </html> >> -> 1 | _ -> 2
function << <html $n$=$v$ a="b" $list:l1$> <body> $list:l2$ </body> </html> >>
    -> 1 | _ -> 2
function << <html $list:l1$> <body> $list:l2$ </body> $list:l3$ </html> >> -> 1 | _ -> 2
(*
(* mais pas : *)
fun << <html $list:l1$> $list:l2$ $list:l3$ </html> >> -> 1
(* ni : *)
fun << <html $list:l1$> $list:l2$ $a$ </html> >> -> 1
(* ni : *)
fun << <html $list:l1$> $list:l3$ <body> $list:l2$ </body> </html> >> -> 1
(* car les $list:l$ sont des listes *)
*)




let _ = print_endline "Essai patterns"
let valeur = "coucou boubou"

let attr = `Class

let dedans = << <div  name="nom" $attr$=$valeur$ id="ident"> hello </div> >>

match << <html> $dedans$   s   $dedans$ </html> >> with
    << <html> $a$ s <div name=$n$ $list:liste$>$list:cc$ </div> </html> >> ->
      (match liste with
        (`Class, s)::ll -> print_string (n^" "^s)
      | _ -> failwith "bouh"
      )
  | _ -> failwith "bah"
;;



let _ = print_endline "\nEssai print"

let print_attrs l = ()

let print_pcdata =  function
    << $str:s$ >> -> print_string s

(*
let print_html =  function
    << <body $list:la$>$list:lt$</body> >> ->
      print_string "<body";
      print_attrs la;
      print_string ">";
      List.iter print_pcdata lt;
      print_string "</body>"

let print = function
    << <html $list:la$> $list:lt$ </html> >> ->
      print_string "<html";
      print_attrs la;
      print_string ">";
      List.iter print_html lt;
      print_string "</html>"

let _ = print << <html> <body> kjl </body> </html> >>

let çacloche = << <body> <html> alala </html> </body> >>
*)

(*

Non :
let _ = print çacloche

*)


