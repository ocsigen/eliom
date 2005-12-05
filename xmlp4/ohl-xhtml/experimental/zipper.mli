(* $Id: zipper.mli,v 1.1 2004/01/25 23:45:03 ohl Exp $ *)

type 'a t

exception Empty_List
exception Right_Margin
exception Left_Margin

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val rev_left : 'a t -> 'a list
val center : 'a t -> 'a
val right : 'a t -> 'a list
    
val step_right : 'a t -> 'a t
val step_left : 'a t -> 'a t
