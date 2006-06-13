(* Copyright Vincent Balat 2005 *)
(** An example of Ocsigen module using Ocsimore *)


open Ocsigen
open Ocsidata
open Ocsipages
open Ocsisav
open Ocsiboxes
open Rights


(******************************************************************)
(* -- Here I populate the database with some examples: *)

let rocsexample =  Ocsipersist.get
    (Ocsipersist.make_persistant_lazy "rocsexample"
       (fun () -> create_resource ()))

let messageslist_number =
  Ocsipersist.get
    (Ocsipersist.make_persistant_lazy "ocsexample_messageslist_number"
       (fun () -> 
	  StringMessageIndexList.dbinsert
	   ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	    [StringMessage.dbinsert 
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un premier message. Blabla blabla blabla blabla. Blabla blabla blabla blabla.";
	     StringMessage.dbinsert 
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un deuxième message. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla.";
	     StringMessage.dbinsert
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un troisième message. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla.";
	     StringMessage.dbinsert 
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un quatrième message. Blabla blabla blabla blabla.";
	     StringMessage.dbinsert
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un cinquième message. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla.";
	     StringMessage.dbinsert
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un sixième message. Blabla blabla blabla blabla. Blabla blabla blabla blabla.";
	     StringMessage.dbinsert
	       ~rights:([anonymoususer],[root],[rocsexample],[rocsexample]) 
	       "Ceci est un septième message. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla. Blabla blabla blabla blabla."])
    )


(* An user *)
let toto_created =
  Ocsipersist.make_persistant_lazy "toto_created"
  (fun () -> 
     ignore (create_user 
	       ~login:"toto" ~name:"Toto" ~password:"titi" ~groups:[users] ());
     true)

(* -- End population of the database with an example *)



(******************************************************************)
(* My boxes *)

(** A box that prints the beginning of a message, with a link to the 
    full message *)
let news_header_box sp key user resource news_page = 
  let msg = StringMessage.dbget user resource key
  in let l = a news_page sp <:xmllist< read >> key
  in << <div> $str:msg$ $l$ <br/> </div> >>

(** A box that prints a list of a message headers *)
let news_headers_list_box sp key user resource news_page = 
  let msglist = 
    List.map 
      (fun n -> news_header_box sp n user resource news_page)
      (StringMessageIndexList.dbget user resource key)
  in << <div>$list:msglist$</div> >>


