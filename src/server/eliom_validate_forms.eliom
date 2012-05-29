(* Eliom_validate_forms
 * Copyright (C) 20010 Simon Castellan
 * For ocsigen
 * http://www.ocsigen.org
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

(* Eliom_validate_forms

   Automatic validation of forms.
   This modules provides a form validation system client-side.
   It does not replace a check on the server, as data coming
   from the client should not be strusted.

   The idea is to reuse the information parameters (Eliom_parameter.params_type)
   to check whether a form's value is "valid". 

   So you should enforce verification on the parameters via the user_type parameter.
*)
(* XX: maybe some things should be factorized out of the example. *)
(** Here is a small example : a login box
    {[
open Eliom_validate_forms
module App =
  Eliom_predefmod.App (
    struct
      let application_name = "eliom_validate_forms_example"
      let params =
        {Eliom_predefmod.default_appl_params with
           Eliom_predefmod.ap_title = "eliom_validate_forms_example";
           Eliom_predefmod.ap_headers =
            [XHTML.M.link ~a:[a_href (uri_of_string "style.css"); a_rel [`Stylesheet]] ()];
           Eliom_predefmod.ap_container =
            Some (None,
                  fun div -> [div])
        }
    end)
;;

module Forms = ValidateForms (App)
let login_form = new_service
  ~path:["test"]
  ~get_params: unit
  ()

let string_guard f = user_type ~to_string:(fun s -> s) 
  ~of_string: (fun s -> if not (f s) then failwith "invalid"; s)

let args = (string_guard ((=) "test") "nick" ** string_guard (fun s -> String.length s >= 8) "password")
let validate = App.register_new_service
  ~path:["validate"]
  ~get_params: args
  (fun sp _ number -> 
    return
      [div [pcdata "You passed!"]])
;;

{client{
  let replace_child node new_children = 
    let rec remove_children () = match Js.Opt.to_option (node##firstChild) with
      | Some child -> Dom.removeChild node child; remove_children ()
      | None -> ()
    in
    remove_children ();
    List.iter (Dom.appendChild node) new_children
}}
;;
let popup ~sp cl content container = 
  {{
    let container = lookup $ magic: container $ in
    Lwt.return
      (replace_child container (XHTML.M.toeltl 
                                  [span ~a:[a_class $ magic : cl $] [pcdata $ magic : content $]]))
  }}


let entry gen_input ?value ?delay ?a ?server_listen ?server_check ~prompt ~sp ~input_type ~name ~id f  =
  label
    [pcdata prompt;
     gen_input ?value ?delay ?a ~input_type ?server_listen ?server_check (fun x -> f x id) name;
     span ~a:[a_id id] []; br ()]

let _ = App.register
  ~service:login_form
  (fun sp () () ->
    let form = Forms.gen_form (fun ~service ~sp f -> App.get_form ~service ~sp f) 
      (validate, (args, unit)) ~sp
      ~on_fail: (popup ~sp ["bad"] "Invalid form" "result")
      (fun gen_input (nick, password) ->
        let entry = entry gen_input ~server_listen: [a_onchange] in
        [fieldset ~a:[a_id "fieldset"]
            [div ~a:[a_id "result"][];
             entry ~prompt: "Nickname:" ~sp ~input_type: `Text ~name: nick ~id: "nick"
               ~server_check: (fun _ param ->
                 Lwt_unix.sleep 2. >>= (fun () -> return (param = "asmanur")))
                (function
                  | `Success -> popup ~sp ["ok"] "Nickname correct"
                  | `Failure -> popup ~sp ["bad"] "Invalid nickname"
                  | `Loading -> popup ~sp [] "Waiting…");
             entry ~prompt: "Password:" ~sp ~input_type: `Password ~name: password ~id: "password"
               (function
                 | `Success -> popup ~sp ["ok"] "valid"
                 | `Failure -> popup ~sp ["bad"] "Invalid password (must be >= 8 characters long)"
                 | `Loading -> popup ~sp [] "");
            App.string_input ~input_type: `Submit ~value: "Submit" ()]])
    in
    return [div [form]])
]}
*)
open Lwt
open XHTML.M
open Eliom_parameter
{client{
  open Lwt
  open Dom_html
  open Eliom_client
  let lookup ?error name  = 
    Js.Opt.get
      (document##getElementById (Js.string name))
      (fun () -> failwith (match error with Some s -> s | None -> name))
  let coerce f arg = 
    Js.Opt.get (f arg) (fun () -> failwith "Invalid element")
      
(* We store some data about the forms in the page, client-side. *)
(* For each form, we have a list of (param_name, validation_code),
   that is used when we check that the form is valid before submission. *)
  let inputs = ref []
    
(* Adds a field to the input. Called in [gen_input] *)
  let set_field (form: string) (field: string) valid = 
    let found = ref false in
    inputs := List.map (fun (form', l) -> 
      if form' = form then
        (found := true; (form', (field, valid) :: l))
      else
        (form', l)) !inputs;
    if not !found then
      inputs := (form, [field, valid]) :: !inputs
      
(* For a given form, returns the list of 
   the inputs that are not valid *)
  let get_invalids form_name =
    Lwt_list.filter_p (fun (name, valid) -> Js.Unsafe.variable valid >|= not)
      (try List.assoc form_name !inputs with Not_found -> [])
    >|= List.map fst
}}

module ValidateForms (Appl :  Eliom_predefmod.XHTMLFORMSSIG ) = struct
  (** Status of a validation :
      - Loading : used to display a nice message for validation
      that may take time : should be empty for fast checks. 
      - Success : when the input is valid
      - Failure : when the input is invalid
  *)
  module Appl = struct include Appl end
  type status = [ `Loading | `Success | `Failure ]

(** Generate a form with automatic validation.
    Arguments :
    - on_fail : an optional javascript code (of type unit Lwt.t) that is
    executed when the whole form is checked but that there is some invalid inputs. 
    - form_name : an optional name to distinguish between several forms in the same page.
    You should specify one if there is more that one form in your page !
    - create : the function from Appl to build the form (post_form or get_form)
    - a pair (service, args) where service is an eliom service and args
    are the post parameters of the service
    - the server params
    - a function that is called to build the form. In the same way
    than Eliom_predefmod.X.post_form takes a function that builds the form
    gen_form does the same, except that the function does take on top of the
    parameter names, a function, [gen_input] used to generate an input checking its contents.

    gen_input takes several arguments :
    - the optional value of the input
    - delay : optional time (defaults to 100 ms) to wait before checking an input's validity.
    - an optional list of attributes
    - a mandatory input_type
    - an optional list of events to listen on for server-side check (eg. a_onchange, etc.)
    - an optional function, checking the contents to see if it is valid. It default
    to the function specified in the arguments of the service. But sometimes, you want
    to do something that takes time : for instance checking that an user exists in database.
    - A function f (server-side) that takes a status and should generate the corresponding 
    javascript code (of type unit Lwt.t).
    - name : the parameter name.
*)

(* TODO:
   - provides support for client-side check. We need to wait for 
   a better support of the inline javascript code
   - for now the form checking is done by clicking on a link
   because we can't listen on the onsubmit event of a form
   thanks to eliom
   - We use Eliom_service.set_on_load that means we can be replaced by any user
   script and that we may erase one of his, eliom doesn't provide a add_on_load function
   - We use some getElementById to get an input's contents. This generates htmlcode with
   a lot of useless id's. This should be changed as we can express things like that
   {{ fun evt self -> ... }}
   - Do some wrappers : gen_get_form, gen_lwt_get_form, ...

*)
  let get_id =
    let k = ref 0 in fun () -> incr k; !k
  let gen_form ?on_fail ?(form_name = "form") create 
      (service, (get, post)) ~sp form_contents = 
    let args = get ** post in
    let submit = {{
      get_invalids $ magic : form_name $ >>= fun invalids ->
      if invalids <> [] then
        (match $ magic : on_fail $ with
          | Some s -> Firebug.console##log (String.concat " " invalids);
            Js.Unsafe.variable s
          | None -> return ())
      else return ()
    }} 
    in
  (* List of javascript code to execute on startup.
     Basically it fills the inputs list below 
  *)
    let scripts = ref [] in
    let gen_input ?value ?(delay = 0.1) ?(a = []) ~input_type 
        ?(server_listen = []) ?server_check f name =
      let id = "__eliom_form_" ^ form_name ^ "__" ^ 
        string_of_int (get_id ()) in
      let from_string, to_string = 
        match walk_parameter_tree (Obj.magic name) args with
          | Some a -> a
          | None -> failwith "Invalid name"
      in
      let check = match server_check with
        | Some f -> f
        | None -> 
          fun _ arg -> return (try ignore (from_string arg); true with _ -> false)
      in
    (* We generate a service dedicated to checking that this parameter is valid.
       It may not be the better way to do this, but eh.
    *)
      let service = Eliom_predefmod.Ocaml.register_new_post_coservice'
        ~post_params: (string "params")
        ~sp
        (fun sp _ arg -> check sp arg)
      in
    (* We use a trick to provide client-side functions in javascript code.
       As we need to evaluate [f] on only three values, we do this on the server
       and send the resulting javacsript code to the client.
       We should use client functions as they are available *)
      let handler = 
        let onloading, onsuccess, onfailure = f `Loading, f `Success, f `Failure in
        {{
          let elem = coerce CoerceTo.input (lookup $ magic : id $) in
          let exec s = Js.Unsafe.variable s in
          exec $ magic:onloading $ >>= fun () ->
          Lwt_js.sleep $ magic: delay $ >>= (fun () ->
            call_caml_service ~service: $ magic:service $ ~sp: $ sp:sp $ ()
              (Js.to_string elem##value) >>= (fun b -> 
                Lwt.catch
                  (fun () -> if b then exec $ magic : onsuccess $ else exec $ magic : onfailure $)
                  (fun e -> return ())
               >>= (fun _ -> return b)))
        }}
      in
      let () = 
        scripts :=
          {{ set_field $ magic : form_name $ $ magic : name $ $ magic : handler $ }} ::
          !scripts in
      let a = a_id id :: List.map (fun x -> x handler) server_listen @ a in
      Appl.user_type_input to_string ?value ~input_type ~a ~name ()
    in
    let form_contents = form_contents gen_input in 
    let form = create
      ~service
      ~sp
      form_contents
    in (* so that side-effects (scripts-filling) take place *)
    let _ = Eliom_service.set_on_load ~sp 
      (String.concat ";\n" !scripts)
    in
    div [XHTML.M.a ~a:[a_href (uri_of_string "#"); a_onclick submit] [pcdata "Check the form"] ;
         form]
  ;;    
end

