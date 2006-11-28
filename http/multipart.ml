(* This code is inspired by mimestring.ml from OcamlNet *)
(* Copyright Gerd Stolpmann, Patrick Doane *) 
(* Modified for Ocsigen/Lwt by Nataliya Guts and Vincent Balat *)

module S = Netstring_pcre
open Lwt
open Ocsistream

exception Multipart_error of string

let cr_or_lf_re = S.regexp "[\013\n]";;

let header_stripped_re =
  S.regexp "([^ \t\r\n:]+):[ \t]*((.*[^ \t\r\n])?([ \t\r]*\n[ \t](.*[^ \t\r\n])?)*)[ \t\r]*\n";;

let header_unstripped_re =
  S.regexp "([^ \t\r\n:]+):([ \t]*.*\n([ \t].*\n)*)";;
(* This much simpler expression returns the name and the unstripped 
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
            raise (Multipart_error "Mimestring.scan_header");
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
                raise (Multipart_error "Mimestring.scan_header")
          end
  in
  parse_header i0 []
;;


let read_header ?downcase ?unfold ?strip (s : Ocsistream.stream) =
  let rec find_end_of_header s =
    catch
      (fun () ->
        let b = Ocsistream.current_buffer s in
        (* Maybe the header is empty. In this case, there is an empty line
         * right at the beginning
         *)
        match S.string_match empty_line_re b 0 with
          Some r ->
            return (s, (S.match_end r))
        | None ->
            (* Search the empty line: *)
            return
              (s, (S.match_end (snd (S.search_forward end_of_header_re b 0))))
      )
      (function
          Not_found -> 
            Ocsistream.enlarge_stream s >>= 
            (function
                Finished _ -> fail Stream_too_small
              | Cont (stri, long, _) as s -> find_end_of_header s)
        | e -> fail e)
  in
  find_end_of_header s >>= (fun (s, end_pos) ->
    let b = Ocsistream.current_buffer s in
    let header, _ = 
      scan_header ?downcase ?unfold ?strip b ~start_pos:0 ~end_pos 
    in
    Ocsistream.skip s end_pos >>= 
    (fun s -> return (s, header)))
;;


let lf_re = S.regexp "[\n]";;


let read_multipart_body decode_part boundary (s : Ocsistream.stream) =

  let rec search_window s re start =
    try
      return (s, snd (S.search_forward re (Ocsistream.current_buffer s) start))
    with
      Not_found -> 
        Ocsistream.enlarge_stream s >>=
        (function
            Finished _ -> fail Stream_too_small
          | Cont (stri, long, _) as s -> search_window s re start)
  in
  let search_end_of_line s k =
    (* Search LF beginning at position k *)
    catch
      (fun () -> (search_window s lf_re k) >>= 
        (fun (s,x) -> return (s, (S.match_end x))))
    (function
        Not_found ->
            fail (Multipart_error 
                    "read_multipart_body: MIME boundary without line end")
        | e -> fail e)
  in

  let search_first_boundary s =
    (* Search boundary per regexp; return the position of the character
     * immediately following the boundary (on the same line), or
     * raise Not_found.
     *)
    let re = S.regexp ("\n--" ^ S.quote boundary) in
    (search_window s re 0) >>= (fun (s, x) -> return (s, (S.match_end x)))
  in

  let check_beginning_is_boundary s =
    let del = "--" ^ boundary in
    let ldel = String.length del in
    Ocsistream.stream_want s ldel >>= (function
        Finished _ as str2 -> return (str2, false)
      | Cont (ss, long, f) as str2 -> 
          return (str2, ((long >= ldel) && 
                         (String.sub ss 0 ldel = del))))
  in

  let rec parse_parts s uses_crlf =
    (* PRE: [s] is at the beginning of the next part. 
     * [uses_crlf] must be true if CRLF is used as EOL sequence, and false
     *    if only LF is used as EOL sequence.
     *)
    let delimiter = (if uses_crlf then "\r" else "" ) ^ "\n--" ^ boundary in
    Ocsistream.substream delimiter s >>=
    decode_part >>= 
    (fun (y,s) -> 
      (* Now the position of [s] is at the beginning of the delimiter. 
       * Check if there is a "--" after the delimiter (==> last part)
       *)
      let l_delimiter = String.length delimiter in
      Ocsistream.stream_want s (l_delimiter+2) >>= (fun s ->
        let last_part = match s with
          Finished _ -> false
        | Cont (ss, long, f) ->
            (long >= (l_delimiter+2)) &&
            (ss.[l_delimiter] = '-') && 
            (ss.[l_delimiter+1] = '-')
        in
        if last_part then return [ y ]
        else begin
          search_end_of_line s 2 >>=  (* [k]: Beginning of next part *)
          (fun (s, k) -> Ocsistream.skip s k >>=
            (fun s -> parse_parts s uses_crlf >>= 
              (fun l -> return (y :: l))))
        end ))
  in

  (* Check whether s directly begins with a boundary: *)
  check_beginning_is_boundary s >>= (fun (s,b) -> if b then begin
    (* Move to the beginning of the next line: *)
    search_end_of_line s 0 >>= (fun (s, k_eol) ->
      let uses_crlf = (Ocsistream.current_buffer s).[k_eol-2] = '\r' in
      Ocsistream.skip s k_eol >>=
      (* Begin with first part: *)
      (fun s -> parse_parts s uses_crlf))
  end
  else begin
    (* Search the first boundary: *)
    catch (fun () -> 
      search_first_boundary s >>= (fun (s, k_eob) ->   (* or Not_found *)
      (* Printf.printf "k_eob=%d\n" k_eob; *)
      (* Move to the beginning of the next line: *)
        search_end_of_line s k_eob >>= (fun (s, k_eol) ->
          let uses_crlf = (Ocsistream.current_buffer s).[k_eol-2] = '\r' in
          (* Printf.printf "k_eol=%d\n" k_eol; *)
          Ocsistream.skip s k_eol >>=
          (* Begin with first part: *)
          (fun s -> parse_parts s uses_crlf))))
      (function 
          Not_found ->
            (* No boundary at all: The body is empty. *)
            return []
        | e -> fail e)  
  end)
;;


let scan_multipart_body_from_stream s ~boundary ~create ~add ~stop =
  let decode_part stream =
    read_header stream >>= (fun (s, header) ->
      let p = create header in
      let rec while_stream size = function
          Finished None -> return (size, empty_stream None)
        | Finished (Some ss) -> return (size, ss)
        | Cont (stri, long, f) ->
            let size2 = Int64.add size (Int64.of_int long) in
            let max = Ocsiconfig.get_maxuploadfilesize () in
            if
              (match max with
                None -> false
              | Some m ->
                  (Int64.compare size2 m) > 0)
            then 
              fail (Ocsimisc.Ocsigen_Request_interrupted
                      Ocsimisc.Ocsigen_Request_too_long)
            else
              if stri = ""
              then f () >>= while_stream size
              else ((* catch 
                       (fun () -> 
                         add p stri)
                       (fun e -> f () >>= 
                         Ocsistream.consume >>= 
                         (fun () -> fail e)) *)
                  add p stri >>= 
                f >>= 
                while_stream size2)
      in 
      catch 
        (fun () -> while_stream Int64.zero s >>= 
          (fun (size, s) -> stop size p >>= (fun r -> return (r,s))))
        (function
            error -> stop Int64.zero p >>= (fun _ -> fail error)))
  in
  catch
    (fun () ->
      (* read the multipart body: *)
      read_multipart_body decode_part boundary s >>=
      (fun _ -> return ()))
    (function
        Stream_too_small -> fail Ocsimisc.Ocsigen_Bad_Request
      | e -> fail e)
;;

