open Lwt.Syntax
module Browser = Eliom_test_server.Browser
module B = Eliom.Comet_base

type 'a message = Data of 'a | Full | Closed

(* Values are sent url-encoded and marshalled, wrapped for the client. *)
let unmarshal s = snd (Marshal.from_string (Eliom.Lib.Url.decode s) 0 : _ * _)

let message decode = function
  | B.Data d -> Data (decode d)
  | B.Full -> Full
  | B.Closed -> Closed

exception State_closed
exception Comet_error of string
exception Timeout

let answer (r : Browser.response) =
  if r.status <> 200 then Printf.ksprintf failwith "Comet: status %d" r.status;
  match Deriving_Json.from_string B.answer_json r.body with
  | B.State_closed -> raise State_closed
  | B.Comet_error e -> raise (Comet_error e)
  | B.Timeout -> raise Timeout
  | a -> a

let params ?(idle = false) (c : Comet_info.t) request =
  c.post_params
  @ [c.request_param, Deriving_Json.to_string B.comet_request_json request]
  @ if idle then [c.idle_param, "on"] else []

let post ?idle tab c request =
  let+ r = Tab.post tab c.Comet_info.url (params ?idle c request) in
  answer r

let commands tab c commands =
  let+ a = post tab c (B.Stateful (B.Commands commands)) in
  match a with
  | B.Stateful_messages [||] -> ()
  | _ -> failwith "Comet: unexpected answer to commands"

let register tab c = commands tab c [|B.Register c.Comet_info.channel|]
let close tab c = commands tab c [|B.Close c.Comet_info.channel|]

let request ?idle tab c n =
  let+ a = post ?idle tab c (B.Stateful (B.Request_data n)) in
  match a with
  | B.Stateful_messages m ->
      Array.to_list (Array.map (fun (id, d) -> id, message unmarshal d) m)
  | _ -> failwith "Comet: unexpected answer to a request of data"

let request_stateless browser (c : Comet_info.t) position =
  let+ r =
    Browser.post browser c.url
      (params ~idle:true c (B.Stateless [|c.channel, position|]))
  in
  match answer r with
  | B.Stateless_messages m ->
      Array.to_list
        (Array.map
           (fun (id, d) -> id, message (fun (s, i) -> unmarshal s, i) d)
           m)
  | _ -> failwith "Comet: unexpected answer to a stateless request"
