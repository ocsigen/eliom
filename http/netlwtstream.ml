 (* Using code from OcamlNet: netchannels.ml, netstream.ml *)
 
 (* Main differences with the original version:
  * - ?init:string argument for input_stream creation
  *    that is added to s_netbuf if present
  * - non-blocking i/o 
  *)

open Netchannels;;

module S = Netstring_pcre
open Lwt

class type in_descr_channel = 
object 
   method pos_in : int
   method close_in : unit -> unit
   method input : string -> int -> int -> int Lwt.t
   method really_input : string -> int -> int -> unit Lwt.t
   method input_char : unit -> char Lwt.t
   method input_line : unit -> string Lwt.t
   method input_byte : unit -> int Lwt.t
end

class input_descr (sock:Lwt_unix.descr) : in_descr_channel =
object (self)
  val ch = sock
  val mutable sock_pos = 0
  val mutable closed = false

  method private complain_closed: 'a.unit -> 'a Lwt.t = fun () ->
      fail Netchannels.Closed_channel

  method input buf pos len =
      let buf_len = String.length buf in
      if pos<0 || len>buf_len || (pos + len) > buf_len 
      then fail (Invalid_argument "input_descr # input")
      else  begin
      if closed then self # complain_closed ()
      else begin
      (Lwt_unix.read sock buf pos len) >>=
      (fun n -> 
      (* print_endline ("buf="^String.sub buf pos n); *)
      sock_pos <- sock_pos + n;
      if n=0 && len>0 then fail End_of_file else return n)
      end end

  method really_input buf pos len = 
      let buf_len = String.length buf in
      if pos<0 || len>buf_len || (pos + len) > buf_len 
          then fail (Invalid_argument "input_descr # really_input") else
      ((self # input buf pos len) >>=
      (fun n -> if n <> len then fail End_of_file else return ()))

  method input_char() =
  print_endline "ch.charinp;";
      if closed then self # complain_closed()
      else begin
      let c = String.create 1 in
      (Lwt_unix.read sock c 0 1) >>= 
      (fun n -> 
      if n = 0 then fail End_of_file else (*(
      sock_pos <- sock_pos + 1;*)
      return c.[0]
      ) end
  
  method input_line() =
  print_endline "ch.inpline;";
      if closed then fail Netchannels.Closed_channel(*self # complain_closed()*) else begin
      let buf = Buffer.create 80 in
      let rec while_not_lf c = 
        if c <> '\n' then begin
          Buffer.add_char buf c;
          (self # input_char ()) >>= (fun ch -> while_not_lf ch)
	end else return () in
      (self # input_char ()) >>= 
      (fun c -> catch (fun () -> while_not_lf c)
               (function End_of_file -> return () | e -> fail e)) >>=
      (fun () -> return (Buffer.contents buf))
      end
       
  method input_byte() =
      (self # input_char()) >>= (fun c -> return (Char.code c))

  method close_in() =
      if closed then raise Netchannels.Closed_channel (*self # complain_closed()*);
      Lwt_unix.shutdown sock;
      closed <- true

  method pos_in =
      if closed then raise Netchannels.Closed_channel(*self # complain_closed()*);
      sock_pos
end
;;

class type in_descr_stream =
object
  inherit input_descr
  method block_size : int
  method window : Netbuffer.t
  method want : int -> unit Lwt.t
  method want_another_block : unit -> unit Lwt.t
  method window_length : int
  method window_at_eof : bool
  method skip : int -> unit Lwt.t
end


class virtual input_methods init_s_netbuf =
object(self)
  val mutable s_pos = 0
  val mutable s_at_eof = false
  val s_netbuf = init_s_netbuf
  val mutable s_closed = false


  method virtual want : int -> unit Lwt.t

  method virtual want_another_block : unit -> unit Lwt.t

  method virtual window_length : int

  method virtual input : string -> int -> int -> int Lwt.t


  (* The following input methods base all on [input] *)

  method really_input buf pos len =
    if s_closed then raise Netchannels.Closed_channel;
    let rec read p =
      (self # input buf (pos+p) (len-p)) >>= (fun l ->
      let p' = p + l in
      if p' = len then
	return ()
      else (
	if l=0 then fail Sys_blocked_io else
	read p'
      ))
    in
    self # want len >>=  (* may raise Buffer_underrun *)
    (fun () -> read 0)


  method input_char () =
    let s = String.create 1 in
    self # really_input s 0 1 >>=
    (fun () -> return s.[0])


  method input_byte () =
    let s = String.create 1 in
    self # really_input s 0 1 >>=
    (fun () -> return (int_of_char s.[0]))


  method input_line () =
    (* CHECK: Are the different end of line conventions important here? *)
    let rec find_eol() =
      catch (fun () ->
	return (Netbuffer.index_from s_netbuf 0 '\n'))    (* or Not_found *)
      (function 
	  Not_found ->
	    if not s_at_eof then begin
	      self # want_another_block() >>=  (* may raise Buffer_underrun *)
	      (fun () -> find_eol())
	    end
	    else return self#window_length
	  | e -> fail e)
    in
    if s_closed then fail Netchannels.Closed_channel else begin
    find_eol() >>= (fun n ->
    if n >= self#window_length then begin
      if n = 0 then fail End_of_file else
      let s = String.create n in
      self#really_input s 0 n >>=
      (fun () -> return s)
    end
    else begin
      let s = String.create n in
      self#really_input s 0 n >>= (fun () -> 
      self#input_char()) >>= (fun _ ->    (* '\n' *)
      return s)
    end) end
    

  method pos_in =
    if s_closed then raise Netchannels.Closed_channel;
    s_pos


end


class input_stream ?init ?len ?(block_size = 4096) in_ch  =
object (self)
  val s_channel = (in_ch : in_descr_channel)
  val s_maxlength = len
  val s_blocksize = block_size
  val mutable s_underrun = false

  inherit input_methods (Netbuffer.create block_size)

  (* Note: This implementation must even work if [in_ch] is a pipe,
   * and raises Buffer_underrun from time to time. This may happen
   * at inconvenient situations. In this case the flag s_underrun stores 
   * whether an underrun happened, and should be reported later.
   *)

  initializer 
    (match init with
    Some s -> Netbuffer.insert_string s_netbuf 0 s
    | _ -> ());
    try
      Lwt_unix.run (self # want_minimum())   (* may raise Buffer_underrun *)
    with
	Buffer_underrun ->
	  s_underrun <- true

  method private debug msg =
    prerr_endline (msg ^ ": s_pos=" ^ string_of_int s_pos ^ 
		   " s_at_eof=" ^ string_of_bool s_at_eof ^ 
		   " buflen=" ^ string_of_int (Netbuffer.length s_netbuf) ^
		   " s_closed=" ^ string_of_bool s_closed);

  method block_size = s_blocksize
    (* The block size is a static property, so never raise Closed_channel *)

  method window = 
    if s_closed then raise Netchannels.Closed_channel;
    s_netbuf

  method window_length = 
    if s_closed then raise Netchannels.Closed_channel;
    Netbuffer.length s_netbuf

  method window_at_eof = 
    if s_closed then raise Netchannels.Closed_channel;
    s_at_eof

  method want_another_block() =
    if s_closed then fail Netchannels.Closed_channel else begin
    if not s_at_eof then begin
      (* How much are we allowed to read? *)
      let m =
	match s_maxlength with
	    None   -> s_blocksize
	  | Some l -> min (l - s_pos - Netbuffer.length s_netbuf) s_blocksize
      in
      assert(m >= 0);
      (* Try to read m bytes: *)
      let rec read_block k =
      self # debug ("read_block "^string_of_int k);
	if k < m then
	  let temp = String.create (m-k) in
	  (s_channel # input temp 0 (m-k)) >>= 
	  (fun n -> Netbuffer.add_sub_string s_netbuf temp 0 n;
	        (* may raise End_of_file, Buffer_underrun *)
	  ( if n > 0 then
	      read_block (k+n)
	    else
	      fail Sys_blocked_io
	  ))
	else
	  return ()
      in
      catch (fun () -> 
	if m=0 then
	  (* Artificial EOF because len is reached *)
	   begin s_at_eof <- true; return () end
	else
	  read_block 0)
      (function
	  End_of_file ->
	    return (s_at_eof <- true) | e -> fail e)
    end else return () end
    (* self # debug "after stream#want_another_block"; *)
    (* Unix.sleep 1; *)


  method want n =
    if s_closed then fail Netchannels.Closed_channel else begin
    let rec while_not_all () = 
      if not s_at_eof && Netbuffer.length s_netbuf < n then
      (self # want_another_block() >>= (fun () -> while_not_all ()))
      else return () in
    while_not_all () end


  method private want_minimum() =
    self # want s_blocksize


  method skip len =
    if s_closed then fail Netchannels.Closed_channel else
    let rec read len =
      if len > 0 then begin
	let k = min (Netbuffer.length s_netbuf) len in
	Netbuffer.delete s_netbuf 0 k;
	s_pos <- s_pos + k;
	self # want_minimum() >>=    (* may raise Buffer_underrun *)
	(fun () -> if k > 0 then read (len - k) else return ())
      end else return ()
    in
    read len


  method input buf pos len =
    if s_closed then fail Netchannels.Closed_channel else begin
    (if s_underrun then (
      self # want_minimum() >>= (* may raise Buffer_underrun *)
      (fun () -> s_underrun <- false; return ())) else return ()) >>=
    (* Assertion: Either window length >= minimum, or eof *)
    (fun () -> let len' = min len (Netbuffer.length s_netbuf) in
    Netbuffer.blit s_netbuf 0 buf pos len';
    Netbuffer.delete s_netbuf 0 len';
    s_pos <- s_pos + len';
    ( catch 
	(fun () -> self # want_minimum())  (* may raise Buffer_underrun *)
      (function
	  Buffer_underrun ->
	    s_underrun <- true; return () | e -> fail e)
    ) >>=
    (fun () -> if len'=0 && len>0 then fail End_of_file else
    return len'))
    end


  method close_in () =
    if s_closed then raise Netchannels.Closed_channel;
    s_channel # close_in();
    s_closed <- true;


end


(*
let find_prefix s1 pos len s2 =
  (* Checks where a non-empty prefix of [s2] occurs at the end of the substring
   * of [s1] beginning at [pos] with length [len]. The function returns 
   * the position [p] of the prefix in [s1]. 
   * The function raises Not_found if it does not find a prefix.
   * POSTCONDITION:
   * - s1[p..p+n-1] = s2[0..n-1] for some biggest n, n <= String.length s2
   *   "The string s1 contains the prefix of s2 at position p, and the
   *   prefix has the maximum length n."
   * - n < String.length s2 ==> p+n = String.length s1
   *   "If the prefix is a proper prefix, it occurs at the end of s1"
   *)
  assert(String.length s2 > 0);
  let l1 = min (String.length s1) (pos+len) in
  let l2 = String.length s2 in
  let s2c0 = s2.[0] in
  let rec check_rec p k =
    k >= l2 || p+k >= l1 || (s1.[p+k] = s2.[k] && check_rec p (k+1)) in
  let rec search_rec p =
    if p >= l1 then raise Not_found;
    let p' = String.index_from s1 p s2c0 in  (* or Not_found *)
    if p' >= l1 then raise Not_found;
    if check_rec p' 0 then
      p'
    else
      search_rec (p'+1)
  in
  search_rec pos
;;
*)


class sub_stream ?len ?delimiter in_stream =
object(self)
  val s = (in_stream : in_descr_stream)
  val mutable s_winlen = 0
  val mutable s_del = None    (* initialized below *)
  val s_len = len
  val mutable s_underrun = false

  inherit input_methods (in_stream # window)

  initializer
    (match delimiter with
	 Some "" -> invalid_arg "new Netstream.sub_stream";
       | Some d  -> s_del <- Some(d, Netaux.KMP.make_pattern d)
       | None    -> s_del <- None
    );
    (match s_len with 
	 Some l -> if l<0 then invalid_arg "new Netstream.sub_stream";
       | None   -> ()
    );
    try
      Lwt_unix.run (self # want_minimum())
    with
	Buffer_underrun ->
	  s_underrun <- true

  method block_size = s # block_size

  method window = 
    if s_closed then raise Netchannels.Closed_channel;
    s_netbuf

  method window_length = 
    if s_closed then raise Netchannels.Closed_channel;
    s_winlen

  method window_at_eof = 
    if s_closed then raise Netchannels.Closed_channel;
    s_at_eof


  method private compute_winlen() =
    (* sets [s_winlen], [s_at_eof], and returns whether the current window
     * is "ambigous" (it is not clear if the stream does end or does not
     * end)
     *)
    let ambigous = ref false in
    let w = s#window in
    let wlen = s#window_length in
    let weof = s#window_at_eof in
    begin match s_del with
	None -> 
	  s_winlen <- wlen;
	  s_at_eof <- weof;
      | Some(d,pat) ->
	  let p = Netaux.KMP.find_pattern
		    pat ~len:wlen (Netbuffer.unsafe_buffer w) in
	  if p >= wlen then begin
	    (* Delimiter d does not occur in the buffer *)
	    s_winlen <- wlen;
	    s_at_eof <- weof;
	  end
	  else 
	    if (p + String.length d) > wlen then begin
	      (* Case: prefix is a proper prefix *)
	      ambigous := not weof;
	      s_winlen <- wlen;
	      s_at_eof <- weof;
	    end
	    else begin
	      (* Case: [d] occurs in the window *)
	      s_winlen <- p;
	      s_at_eof <- true;
	    end
    end;
    begin match s_len with
	None ->
	  ()
      | Some l ->
	  if l - s_pos < s_winlen then begin
	    ambigous := false;
	    s_winlen <- l - s_pos;
	    s_at_eof <- true;
	  end
    end;
    !ambigous


  method want_another_block() =
    if s_closed then fail Netchannels.Closed_channel else begin
    let rec while_compute () = 
      if self # compute_winlen() then
        (s # want_another_block() >>= (fun () -> while_compute ()))
	else return () in(* may raise Buffer_underrun *)
    s # want_another_block() >>=  (fun () -> while_compute ()) (* may raise Buffer_underrun *)
    end

  method want n =
    if s_closed then fail Netchannels.Closed_channel else begin
    let rec while_not_eof () = 
      if not s_at_eof && s_winlen < n then
        (self # want_another_block() >>= (fun () -> while_not_eof ())) (* may raise Buffer_underrun *)
        else return () in
    while_not_eof ()
    end

  method private want_minimum() =
    (if self # compute_winlen() then
      self # want_another_block() else return ()) >>=  (* may raise Buffer_underrun *)
    (fun () -> self # want s#block_size)


  method skip len =
    if s_closed then fail Netchannels.Closed_channel else 
    let rec read len =
      if len > 0 then begin
	let k = min s_winlen len in
	s # skip k >>=              (* may raise Buffer_underrun *)
	(fun () -> s_pos <- s_pos + k;
	self # want_minimum()) >>=   (* may raise Buffer_underrun *)
	(fun () -> if k > 0 then read (len - k) else return ())
      end else return ()
    in
    read len


  method input buf pos len =
    if s_closed then fail Netchannels.Closed_channel else begin
    (if s_underrun then (
      self # want_minimum() >>=  (* may raise Buffer_underrun *)
      (fun () -> return (s_underrun <- false))
    ) else return ()) >>= (fun () ->
    (* Assertion: Either window length >= minimum, or eof *)
    let len' = min len s_winlen in
    Netbuffer.blit s_netbuf 0 buf pos len';
    s # skip len' >>=              (* never raises Buffer_underrun *)
    (fun () -> s_pos <- s_pos + len';
    ( catch
	(fun () -> self # want_minimum())
      (function 
	  Buffer_underrun ->
	    return (s_underrun <- true) | _ -> return ()
    ))) >>= (fun () ->
    if len'=0 && len>0 then fail End_of_file
    else return len')) end


  method close_in () =
    if s_closed then raise Netchannels.Closed_channel;
    s # close_in();
    s_closed <- true;

end
