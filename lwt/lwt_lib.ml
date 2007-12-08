(* Ocsigen
 * http://www.ocsigen.org
 * lwt_lib.ml Copyright (C) 2007 Pierre Clairambault
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

open Weak
open Unix
open Lwt

let switch_time = 30.

module WeakHashtbl = Make(  
  struct    
    type t = string*Unix.host_entry*float
    let equal = (fun (a,b,c) -> fun (a',b',c') -> a=a')  
    let hash = fun (a,b,c) -> Hashtbl.hash a  
  end
)

open WeakHashtbl

let keeper : (((string*Unix.host_entry*float) list) * 
                ((string*Unix.host_entry*float) list)) ref = ref ([],[])
let cache = create 0
let dummy_addr : Unix.host_entry = 
  {h_name="dummy"; 
   h_aliases=[||]; 
   h_addrtype=Unix.PF_INET; 
   h_addr_list = [||]}

let cache_find d = match (find cache (d,dummy_addr,0.)) with (_,h,t) -> (h,t)

let switch_thread : unit Lwt.t= 
  let rec switch_worker () = 
    Lwt_unix.sleep switch_time >>= fun () -> 
    (match !keeper with (a,b) -> keeper:=([],a));
    switch_worker ()
  in 
  switch_worker()

let gethostbyname d = 
  Lwt.catch
    (fun _ ->
       let (h,t) = cache_find d 
       and t' = Unix.time () in
       match (t'>t+.60.) with 
	 | true -> 
             (remove cache) (d,h,t);
	     Lwt.fail Not_found
	 | false -> Lwt.return h)
    (function 
       | Not_found -> 
           (Preemptive.detach Unix.gethostbyname d >>= fun h -> 
             let t = Unix.time () in
             let entry = (d,h,t) in
             add cache entry;
             (match !keeper with (a,b) -> keeper:= (entry::a,b)); 
             Lwt.return h)
       | e -> fail e
    )



