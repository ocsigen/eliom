(* Using code from OcamlNet: mimestring.ml *)
module S = Netstring_pcre
open Lwt

let cr_or_lf_re = S.regexp "[\013\n]";;

let header_stripped_re =
  S.regexp "([^ \t\r\n:]+):[ \t]*((.*[^ \t\r\n])?([ \t\r]*\n[ \t](.*[^ \t\r\n])?)*)[ \t\r]*\n";;

let header_unstripped_re =
  S.regexp "([^ \t\r\n:]+):([ \t]*.*\n([ \t].*\n)*)";;
(* This much simpler expression returnes the name and the unstripped 
 * value.
 *)

let empty_line_re =
  S.regexp "\013?\n";;

let end_of_header_re =
  S.regexp "\n\013?\n";;


let scan_header ?(downcase=true) 
                ?(unfold=true) 
		?(strip=false)
		parstr ~start_pos:i0 ~end_pos:i1 =
  let header_re =
    if unfold || strip then header_stripped_re else header_unstripped_re in
  let rec parse_header i l =
    match S.string_match header_re parstr i with
	Some r ->
	  let i' = S.match_end r in
	  if i' > i1 then
	    failwith "Mimestring.scan_header";
	  let name = 
	    if downcase then
	      String.lowercase(S.matched_group r 1 parstr) 
	    else
	      S.matched_group r 1 parstr
	  in
	  let value_with_crlf =
	    S.matched_group r 2 parstr in
	  let value =
	    if unfold then
	      S.global_replace cr_or_lf_re "" value_with_crlf
	    else
	      value_with_crlf
	  in
	  parse_header i' ( (name,value) :: l)
      | None ->
	  (* The header must end with an empty line *)
	  begin match S.string_match empty_line_re parstr i with
	      Some r' ->
		List.rev l, S.match_end r'
	    | None ->
		failwith "Mimestring.scan_header"
	  end
  in
  parse_header i0 []
;;


let read_header ?downcase ?unfold ?strip (s : Netlwtstream.in_descr_stream) =
  let rec find_end_of_header() =
    try
      let b = Netbuffer.unsafe_buffer s#window in
      (* Maybe the header is empty. In this case, there is an empty line
       * right at the beginning
       *)
      begin match S.string_match empty_line_re b 0 with
	  Some r ->
	    return (S.match_end r)
	| None ->
	    (* Search the empty line: *)
	    let _, r = S.search_forward end_of_header_re b 0 in (*or Not_found*)
	    if S.match_end r > s#window_length then fail Not_found 
	    else return (S.match_end r)
      end
    with
	Not_found ->
	  if s#window_at_eof then 
	    fail (Failure "Mimestring.read_header")
	  else (
	  s#want_another_block() >>=
	  (fun () -> find_end_of_header()))
  in
  find_end_of_header() >>= (fun end_pos ->
  let b = Netbuffer.unsafe_buffer s#window in
  let header, _ = scan_header ?downcase ?unfold ?strip b ~start_pos:0 ~end_pos 
  in
  s # skip end_pos >>= 
  (fun () -> return header))
;;
let lf_re = S.regexp "[\n]";;


let read_multipart_body f boundary s =

  let rec search_window re start =
    try
      let i,r = S.search_forward re (Netbuffer.unsafe_buffer s#window) start in
      (* If match_end <= window_length, the search was successful.
       * Otherwise, we searched in the uninitialized region of the
       * buffer.
       *)
      if S.match_end r > s#window_length then fail Not_found
      else return r
    with
	Not_found -> 
	  if s # window_at_eof then fail Not_found else
	  (s#want_another_block() >>=
	  (fun () -> search_window re start))     (* try again with enlarged window *)
  in

  let search_end_of_line k =
    (* Search LF beginning at position k *)
    catch
      (fun () -> (search_window lf_re k) >>= (fun x -> return (S.match_end x)))
    (function
	Not_found ->
	    fail (Failure "read_multipart_body: MIME boundary without line end")
	| e -> fail e)
  in

  let search_first_boundary() =
    (* Search boundary per regexp; return the position of the character
     * immediately following the boundary (on the same line), or
     * raise Not_found.
     *)
    let re = S.regexp ("\n--" ^ S.quote boundary) in
    (search_window re 0) >>= (fun x -> return (S.match_end x))
  in

  let check_beginning_is_boundary() =
    let del = "--" ^ boundary in
    let ldel = String.length del in
    s # want ldel >>= (fun () ->
    let buf = Netbuffer.unsafe_buffer s#window in
    return ((s # window_length >= ldel) && (String.sub buf 0 ldel = del)))
  in

  let rec go_to_eof stream =
    if stream#window_at_eof then
      stream#skip stream#window_length
    else begin
      stream#skip (stream#block_size) >>=
      (fun () -> go_to_eof stream)
    end
  in

  let rec parse_parts uses_crlf =
    (* PRE: [s] is at the beginning of the next part. 
     * [uses_crlf] must be true if CRLF is used as EOL sequence, and false
     *    if only LF is used as EOL sequence.
     *)
    let delimiter = (if uses_crlf then "\r" else "" ) ^ "\n--" ^ boundary in
    let sub_s = new Netlwtstream.sub_stream ~delimiter s in
    f sub_s >>= (fun y -> 
    go_to_eof sub_s >>= (fun () -> 
    (* Now the position of [s] is at the beginning of the delimiter. 
     * Check if there is a "--" after the delimiter (==> last part)
     *)
    let l_delimiter = String.length delimiter in
    s # want (l_delimiter+2) >>= (fun () ->
    let buf = Netbuffer.unsafe_buffer s#window in
    let last_part = 
      (s#window_length >= (l_delimiter+2)) &&
      (buf.[l_delimiter] = '-') && 
      (buf.[l_delimiter+1] = '-') 
    in
    if last_part then begin
      go_to_eof s >>=
      (fun () -> return [ y ])
    end
    else begin
      search_end_of_line 2 >>=  (* [k]: Beginning of next part *)
      (fun k -> s # skip k >>=
      (fun () -> parse_parts uses_crlf >>= (fun l -> return (y :: l))))
    end )))
  in

  (* Check whether s directly begins with a boundary: *)
  check_beginning_is_boundary() >>= (fun b -> if b then begin
    (* Move to the beginning of the next line: *)
    search_end_of_line 0 >>= (fun k_eol ->
    let uses_crlf = (Netbuffer.unsafe_buffer s#window).[k_eol-2] = '\r' in
    s # skip k_eol >>=
    (* Begin with first part: *)
    (fun () -> parse_parts uses_crlf))
  end
  else begin
    (* Search the first boundary: *)
    catch (fun () -> 
      search_first_boundary() >>= (fun k_eob ->   (* or Not_found *)
      (* Printf.printf "k_eob=%d\n" k_eob; *)
      (* Move to the beginning of the next line: *)
      search_end_of_line k_eob >>= (fun k_eol ->
      let uses_crlf = (Netbuffer.unsafe_buffer s#window).[k_eol-2] = '\r' in
      (* Printf.printf "k_eol=%d\n" k_eol; *)
      s # skip k_eol >>=
      (* Begin with first part: *)
      (fun () -> parse_parts uses_crlf))))
    (function 
	Not_found ->
	  (* No boundary at all: The body is empty. *)
	  return []
	| e -> fail e)  
  end)
;;

(*
let scan_multipart_body s ~start_pos ~end_pos ~boundary =
  let decode_part stream =
    let header = read_header stream in
    while not (stream # window_at_eof) do stream # want_another_block() done;
    let body = Netbuffer.sub stream#window 0 stream#window_length in
    header, body
  in

  let l_s = String.length s in
  if start_pos < 0 or end_pos < 0 or start_pos > l_s or end_pos >l_s then
    invalid_arg "Mimestring.scan_multipart_body";
  (* Set up a netstream beginning at ~start_pos and ending at ~end_pos: *)
  let len = end_pos - start_pos in
  let stream =
    new Netlwtstream.input_stream 
      (new Netchannels.input_string ~pos:start_pos ~len s) in
  (* Note: We would save a copy of the string if there was a class combining
   * input_stream and input_string 
   *)
  (* read the multipart body: *)
  read_multipart_body decode_part boundary stream
;;
  

let scan_multipart_body_and_decode s ~start_pos:i0 ~end_pos:i1 ~boundary =
  let parts = scan_multipart_body s i0 i1 boundary in
  List.map
    (fun (params, value) ->
       let encoding =
	 try List.assoc "content-transfer-encoding" params
	 with Not_found -> "7bit"
       in

       (* NOTE: In the case of "base64" and "quoted-printable", the allocation
	* of the string "value" could be avoided.
	*)

       let value' =
	 match encoding with
	     ("7bit"|"8bit"|"binary") -> value
	   | "base64" ->
	       Netencoding.Base64.decode 
	         ~url_variant:false ~accept_spaces:true value
	   | "quoted-printable" ->
	       Netencoding.QuotedPrintable.decode
		 value 
	   | _ ->
	       failwith "Mimestring.scan_multipart_body_and_decode: Unknown content-transfer-encoding"
       in
       (params, value')
    )
    parts
;;
*)

let scan_multipart_body_from_netstream s ~boundary ~create ~add ~stop =
  let decode_part stream =  
    read_header stream >>= (fun header ->
    let p = create header in
    let rec while_length () = 
      let l = stream#window_length in
        if l > 0 then begin
	add p stream 0 l >>=
	(fun () -> stream # skip l >>= (fun () -> while_length ())) 
	end else return () in 
    catch (fun () -> while_length () >>= 
          (fun () -> Lwt_unix.yield () >>= (fun () ->return (stop p))))
    (function
	error ->
	  stop p; fail error))
  in
  (* read the multipart body: *)
  read_multipart_body decode_part boundary s >>=
  (fun _ -> return ())
;;

