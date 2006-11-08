open Lwt

exception Stream_too_small
exception Stream_error of string
exception String_too_large

type stream = 
    Finished of stream option (* If there is another stream following
				 (usefull for substreams) *)
  | Cont of string * int * (unit -> stream Lwt.t)
	(* current buffer, size, follow *)

let empty_stream follow = Finished follow

let new_stream stri f = Cont (stri, (String.length stri), f)

let string_of_stream = 
  let rec aux l = function
      Finished _ -> return ""
    | Cont (s, long, f) -> 
	let l2 = l+long in
	if l2 > Ocsiconfig.get_netbuffersize ()
	then fail String_too_large
	else 
	  (f () >>= 
	   (fun r -> aux l2 r >>=
	     (fun r -> return (s^r))))
  in aux 0

let enlarge_stream = function 
    Finished a -> fail Stream_too_small
  | Cont (s, long, f) ->
      f () >>= 
      (function
	  Finished _ -> fail Stream_too_small
	| Cont (r, long2, ff) -> return (Cont (s^r, long+long2, ff)))

let rec stream_want s len =
 (* returns a stream with at most len bytes read if possible *)
  match s with
    Finished _ -> return s
  | Cont (stri, long, f)  -> if long >= len
  then return s
  else catch
	(fun () -> enlarge_stream s >>= (fun r -> stream_want s len))
	(function
	    Stream_too_small -> return s
	  | e -> fail e)

let current_buffer = function
    Finished _ -> raise Stream_too_small
  | Cont (s, l, _) -> s

let rec skip s k = match s with
  Finished _ -> raise Stream_too_small
| Cont (s, len, f) -> 
    if k <= len
    then return (Cont ((String.sub s k (len - k)), (len - k), f))
    else (enlarge_stream (Cont ("", 0, f)) >>= 
	  (fun s -> skip s (k - len)))

let substream delim s = 
  let ldelim = String.length delim in
  if ldelim = 0 then fail (Stream_error "Empty delimiter")
  else 
    let rdelim = Netstring_pcre.regexp_string delim in
    let rec aux =
      function
	  Finished _ -> raise Stream_too_small
	| Cont (s, len, f) as stre -> 
	    if len < ldelim
	    then enlarge_stream stre >>= aux
	    else try 
	      let p,_ = Netstring_pcre.search_forward rdelim s 0 in
	      return 
		(Cont ((String.sub s 0 p), 
		       p,
		       (fun () -> return 
			   (Finished
			      (Some (Cont ((String.sub s p (len - p)), 
					   (len -p), 
					   f)))))))
	    with Not_found ->
	      let pos = (len + 1 - ldelim) in
	      return
		(Cont ((String.sub s 0 pos),
		       pos,
		       (fun () -> f () >>=
			 (function
			     Finished _ -> fail Stream_too_small
			   | Cont (s', long, f') -> 
			       aux 
				 (Cont ((String.sub s pos (len - pos))^s', 
					(long+len-pos),
					f'))
			 ))))
    in aux s


