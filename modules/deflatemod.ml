(* Ocsigen
 * http://www.ocsigen.org
 * Module deflatemod.ml
 * Copyright (C) 2007 Gabriel Kerneis
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
(*****************************************************************************)
(*****************************************************************************)
(* This module allows to compress output sent by the server                  *)
(*****************************************************************************)
(*****************************************************************************)

open Lwt
open Extensions
open Simplexmlparser
open Ocsiheaders


(* Content-type *)
type compress_choice = No_compress of (string option * string option) list |
                       Compress_only of (string option * string option) list 

let should_compress (t, t') choice_list = 
 let check = function
 |None, None -> true
 |None, Some x' -> x' = t' 
 |Some x, None -> x = t 
 |Some x, Some x' -> x = t && x' = t' 
 in
 match choice_list with
 |Compress_only l -> List.exists check l
 |No_compress l -> List.for_all (fun c -> not (check c)) l

let choice_list = ref (No_compress [])

(** Compression *)

let buffer_size = ref 1024

(*  0 = no compression ; 1 = best speed ; 9 = best compression *)
let compress_level = ref 6

(* Minimal header, by X. Leroy *)
let gzip_header_length = 10 
let gzip_header = String.make gzip_header_length (Char.chr 0)
let () =
  gzip_header.[0] <- Char.chr 0x1F;
  gzip_header.[1] <- Char.chr 0x8B;
  gzip_header.[2] <- Char.chr 8;
  gzip_header.[9] <- Char.chr 0xFF


(* inspired by an auxiliary function from camlzip, by Xavier Leroy *)
type output_buffer =
  {
  stream: Zlib.stream;
  buf: string;
  mutable pos: int;
  mutable avail: int;
  }

(* puts in oz the content of buf, from pos to pos + len ;
 * f is the continuation of the current stream *)
let rec output oz f buf pos len  =
   Messages.debug "--Deflatemod: Entering output to deflate";
   if pos < 0 || len < 0 || pos + len > String.length buf then
           assert false ;

  if oz.avail = 0 then
    (let cont () = output oz f buf pos len in
    Messages.debug "--Deflatemod: Flushing because output buffer is full";
    flush oz cont)
  else (
  (catch
      (fun () -> 
        (Messages.debug "--Deflatemod: Actually deflating...";
        try return(Zlib.deflate oz.stream buf pos len
                                 oz.buf oz.pos oz.avail
                                 Zlib.Z_SYNC_FLUSH)
	    with e -> fail e))
      (function 
         |Zlib.Error(_, _) -> 
                fail (Ocsistream.Stream_error("Error during compression"))
         | e -> fail e)) >>=
  (fun  (_, used_in, used_out) ->
  oz.pos <- oz.pos + used_out;
  oz.avail <- oz.avail - used_out;
  (* If we didn't deflate the whole input or if the buffer is full, continue *)
  if (used_in < len) || (oz.avail = 0) 
  then output oz f buf (pos + used_in) (len - used_in)
  (* otherwise, go to the next part of the stream *)
  else 
    f () >>= (next_cont oz)
  ))

(* Flush oz, ie. produces a new_stream with the content of oz, cleans it
 * and returns the continuation of the stream *)
and flush oz cont =
        let len = oz.pos in
        let s = String.sub oz.buf 0 len in
        Messages.debug "--Deflatemod: Flushing!";
        oz.pos <- 0 ; 
        oz.avail <- String.length oz.buf ;
        if len > 0 then
        return (Ocsistream.new_stream ~len:len s cont)
        else cont ()

and next_cont oz stream = 
  match stream with
  | Ocsistream.Finished None -> 
      Messages.debug "--Deflatemod: End of stream: big cleaning for zlib" ; 
      
      (* loop until there is nothing left to compress and flush *)
      let rec after_flushing () = 
        (* buffer full *)
        if oz.avail = 0 then flush oz after_flushing
        else (
        (* no more input, deflates only what were left because output buffer 
         * was full *)
        let (finished, _, used_out) =
         Zlib.deflate oz.stream oz.buf 0 0 oz.buf oz.pos oz.avail Zlib.Z_FINISH 
        in
        oz.pos <- oz.pos + used_out;
        oz.avail <- oz.avail - used_out;
        if not finished then
            after_flushing ()
        else
            (Zlib.deflate_end oz.stream ; 
            Messages.debug "--Deflatemod: Zlib stream closed, last flush" ;
            flush oz (fun () -> return (Ocsistream.empty_stream None)))) in
      
      flush oz after_flushing
  | Ocsistream.Finished (Some s) -> next_cont oz s
  | Ocsistream.Cont(s,l,f) ->  
      Messages.debug "--Deflatemod: Next part of stream" ; 
      output oz f s 0 l 
 
(* deflate param : true = deflate ; false = gzip (no header in this case) *)
let compress deflate stream = 
  let oz = 
    { stream = Zlib.deflate_init !compress_level deflate ;
      buf=String.create !buffer_size;
      pos = 0;
      avail = !buffer_size
    } in
  if deflate then
  (Messages.debug "--Deflatemod: Preparing to compress with deflate...";
  Ocsistream.new_stream ~len:0 "" (fun () -> next_cont oz stream))
  else
  (Messages.debug "--Deflatemod: Preparing to compress with gzip...";
  Ocsistream.new_stream ~len:gzip_header_length gzip_header 
    (fun () -> next_cont oz stream))




(*****************************************************************************)
(** Extensions may take some options from the config file. 
    These options are written in xml inside the <extension> tag.
   For example:
   <extension module=".../extensiontemplate.cmo">
     <myoption myattr="hello">
        ...
     </myoption>
   </extension>
 *)

let rec parse_contenttypes = function
 |[] -> []
 |(Element ("type",[],[PCData t]))::q -> 
  (Ocsiheaders.parse_mime_type t) :: parse_contenttypes q
 |_ -> raise (Error_in_config_file 
                  "Unexpected element inside contenttype (should be <type>)")


let rec parse_global_config = function
  | [] -> ()
  | (Element ("compress", [("level", l)], []))::ll ->
     let l = try int_of_string l
     with Failure _ -> raise (Error_in_config_file 
         "Compress level should be an integer between 0 and 9") in
     compress_level := if (l <= 9 && l >= 0) then l else 6 ;
     parse_global_config ll
  | (Element ("buffer", [("size", s)], []))::ll ->
     let s = (try int_of_string s
     with Failure _ -> raise (Error_in_config_file 
         "Buffer size should be a positive integer")) in
     buffer_size := if s > 0 then s else 1024 ;
     parse_global_config ll
  | (Element ("contenttype", [("compress", b)], choices))::ll -> 
     let l = (try parse_contenttypes choices 
             with Not_found -> raise (Error_in_config_file 
                  "Can't parse mime-type content")) in
     (match b with
     |"only" -> choice_list := Compress_only l
     |"allbut" -> choice_list := No_compress l
     | _ ->  raise (Error_in_config_file 
       "Attribute \"compress\" should be \"allbut\" or \"only\""))
  | _ -> raise (Error_in_config_file 
                  "Unexpected content inside deflatemod config")

let _ = parse_global_config (Extensions.get_config ())



(*****************************************************************************)
(** Extensions may define new tags for configuring each site.
    These tags are inside <site ...>...</site> in the config file.
        
   For example:
   <site dir="">
     <extensiontemplate module=".../mymodule.cmo" />
   </extension>

   Each extension will set its own configuration options, for example:
   <site dir="">
     <extensiontemplate module=".../mymodule.cmo" />
     <eliom module=".../myeliommodule.cmo" />
     <static dir="/var/www/plop" />
   </extension>

 *)

let parse_config path = function
(*  | Element ("deflate", atts, []) -> () 
   Ici il faut créer un arbre de répertoires en se souvenant les options
   de compression de chaque répertoire.
   cf staticmod par exemple pour page_tree
 *)
  | Element (t, _, _) -> raise (Bad_config_tag_for_extension t)
  | _ -> 
      raise (Error_in_config_file "Unexpected data in config file")




(*****************************************************************************)
(** Function to be called at the beginning of the initialisation phase 
    of the server (actually each time the config file is reloaded) *)
let start_init () =
  ()

(** Function to be called at the end of the initialisation phase *)
let end_init () =
  ()



(*****************************************************************************)
(** A function that will create an error message from the exceptions
    that may be raised during the initialisation phase, and raise again 
    all other exceptions. That function has type exn -> string. Use the 
   raise function if you don't need any. *)
let exn_handler = raise



(*****************************************************************************)
(** The filter function *)
(* We implement Content-Encoding, not Transfer-Encoding *)

type encoding = Deflate | Gzip | Id | Star | Not_acceptable

let qvalue = function Some x -> x |None -> 1.0

let enc_compare e e' = match e,e' with
|(Star,_),(_,_) -> -1 (* star should be at the very end *)
|(_,_),(Star,_) -> 1
|(_,v),(_,v') when v<v' -> 1 (* then, sort by qvalue *)
|(_,v),(_,v') when v>v' -> -1
|(x,_),(x',_) when x=x' -> 0
|(Deflate,_),(_,_) -> 1 (* and subsort by encoding *)
|(_,_),(Deflate,_) -> -1
|(Gzip,_),(_,_) -> 1
|(_,_),(Gzip,_) -> -1
|(Id,_),(_,_) -> 1
|(_,_),(Id,_) -> -1
|_ -> assert false

let rec filtermap f = function
  |[] -> []
  |t::q -> match f t with 
    |Some s -> s::(filtermap f q)
    |None -> filtermap f q

let convert = function
|(Some "deflate",v) -> Some (Deflate, qvalue v)
|(Some "gzip",v)|(Some "x-gzip",v) -> Some (Gzip, qvalue v)
|(Some "identity",v) -> Some (Id, qvalue v)
|(None,v) -> Some (Star, qvalue v)
|_ -> None

(* Follow http's RFC to select the transfert encoding *)
let select_encoding accept_header = 
  let h = List.sort enc_compare (filtermap convert accept_header) in
  let (exclude,accept) = 
    let (e,a) = List.partition (fun x -> snd x = 0.) h in
    (List.map fst e, List.map fst a) in
  let rec aux = function 
  |[] ->
    if ((List.mem Star exclude) || (List.mem Id exclude)) 
    then Not_acceptable else Id 
  |t::q -> 
    if (List.mem t exclude)
    then aux q else t
  in 
  aux accept

exception No_compress

let stream_filter deflate choice contenttype (len, etag, stream, finalize) = 
 try (
   match contenttype with
   | None -> raise No_compress (* il faudrait défaut ? *)
   | Some contenttype ->
       match Ocsiheaders.parse_mime_type contenttype with
       | None, _ | _, None -> raise No_compress (* should never occure? *)
       | (Some a, Some b) when should_compress (a, b) choice ->
           return (None, 
                   (if deflate then "Ddeflatemod" else "Gdeflatemod")^etag, 
                   compress deflate stream, 
                   finalize)
       | _ -> raise No_compress)
 with Not_found | No_compress -> 
   return (None, etag, stream, finalize)

let filter ri res =
  (* TODO: Ici il faut regarder dans l'arbre de configuration
     s'il faut compresser ou pas (et changer choice_list en conséquence) *)
 match select_encoding (Lazy.force(ri.ri_accept_encoding)) with
  |Deflate ->   return
    {res with
     res_headers = ("Content-Encoding","deflate")::res.res_headers;
     res_filter= Some (stream_filter true !choice_list)
   }
  |Gzip ->   return
    {res with
     res_headers = ("Content-Encoding","gzip")::res.res_headers;
     res_filter= Some (stream_filter false !choice_list)
   } 
  |Id|Star -> return res
  |Not_acceptable -> return 
    {res with 
     res_code = Some 406;
     res_send_page= 
       Predefined_senders.send_error ?http_exception:None
   }


(*****************************************************************************)
(** A function that will be called for each virtual host,
   generating two functions: 
    - one that will be called to filter the output
    - one to parse the configuration file. *)
let virtual_host_creator hostpattern = (filter, parse_config)
   (* hostpattern has type Extensions.virtual_hosts
      and represents the name of the virtual host *)
   

(*****************************************************************************)
(** Registration of the extension *)
let _ = R.register_output_filter (* takes a quadruple *)
    (virtual_host_creator,
     start_init,
     end_init,
     exn_handler)

