(* $Id: zipper.ml,v 1.1 2004/01/25 23:45:03 ohl Exp $ *)

exception Empty_List
exception Right_Margin
exception Left_Margin

type 'a t =
    { rev_left : 'a list;
      center : 'a;
      right : 'a list }
      
let of_list = function
  | [] -> raise Empty_List
  | c :: r -> { rev_left = []; center = c; right = r }

let to_list z =
  List.rev_append z.rev_left (z.center :: z.right)

let rev_left z = z.rev_left
let center z = z.center
let right z = z.right

let step_right z =
  match z.right with
  | [] -> raise Right_Margin
  | c :: r -> { rev_left = z.center :: z.rev_left; center = c; right = r }

let step_left z =
  match z.rev_left with
  | [] -> raise Left_Margin
  | c :: l -> { rev_left = l; center = c; right = z.center :: z.right }
