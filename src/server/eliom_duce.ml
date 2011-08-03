(* Ocsigen
 * http://www.ocsigen.org
 * Module Eliom_duce
 * Copyright (C) 2007 Vincent Balat, Alain Frisch
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


open Ocsigen_http_frame
open Ocsigen_http_com
open Lwt
open Ocsigen_senders
open Ocsigen_stream
open XHTML_types_duce
open Ocsigen_extensions
open Eliom_mkforms
open Eliom_mkreg

let str = Ocamlduce.Utf8.make

let add_css (a : html) : html =
  let css =
    {{ <style type="text/css">"\n.eliom_inline {display: inline}\n.eliom_nodisplay {display: none}\n"}}
  in
  {{ match a with <html (al)>el ->
   <html (al)>
      (map* el with <head (al)> el -> [ <head (al)> [ css !el ] ])
   }}


let code_of_code_option = function
  | None -> 200
  | Some c -> c

module Ocamlduce_content =
  struct
    type t = {{ html }}

    let get_etag_aux x =
      Some (Digest.to_hex (Digest.string x))

    let print x =
      let b = Buffer.create 256 in
      XHTML_duce.P.print ~advert:Ocsigen_pervasives.advert ~output:(Buffer.add_string b) x;
      Buffer.contents b

    let get_etag c =
      let x = print (add_css c) in
      get_etag_aux x

    module S = Ocsigen_stream.StringStream

    let result_of_content c =
      let x = print (add_css c) in
      let md5 = get_etag_aux x in
      let default_result = default_result () in
      Lwt.return
        {default_result with
         res_content_length =
            Some (Int64.of_int (String.length x));
         res_content_type = Some "text/html";
         res_etag = md5;
         res_headers= Http_headers.dyn_headers;
         res_stream =
             (S.make (S.put x), None)
       }

  end


module Xhtml_reg_base = struct

  type page = html
  type options = unit
  type return = Eliom_output.http_service
  type result = Ocsigen_http_frame.result

  let result_of_http_result x = x

  let send_appl_content = Eliom_services.XNever

  let send ?options ?charset ?code
      ?content_type ?headers content =
    Ocamlduce_content.result_of_content content >>= fun r ->
      Lwt.return
	{r with
           res_code= code_of_code_option code;
           res_charset= Some "utf-8" (* For Eliom_duce, we impose utf-8 *);
           res_content_type= (match content_type with
                              | None -> r.res_content_type
                              | _ -> content_type
                             );
           res_headers= (match headers with
                         | None -> r.res_headers
                         | Some headers ->
                             Http_headers.with_defaults
                               headers r.res_headers
			);
	}

end

module Xhtml_forms_base = struct

  type uri = Eliom_duce_types.uri
  type pcdata_elt = Eliom_duce_types.pcdata_elt

  type form_elt = Eliom_duce_types.form_elt
  type form_content_elt = Eliom_duce_types.form_content_elt
  type form_content_elt_list = Eliom_duce_types.form_content_elt_list
  type form_attrib_t = Eliom_duce_types.form_attrib_t

  type 'a a_elt = 'a Eliom_duce_types.a_elt
  type 'a a_elt_list = 'a Eliom_duce_types.a_elt_list
  type 'a a_content_elt = 'a Eliom_duce_types.a_content_elt
  type 'a a_content_elt_list = 'a Eliom_duce_types.a_content_elt_list
  type a_attrib_t = Eliom_duce_types.a_attrib_t

  type link_elt = Eliom_duce_types.link_elt
  type link_attrib_t = Eliom_duce_types.link_attrib_t

  type script_elt = Eliom_duce_types.script_elt
  type script_attrib_t = Eliom_duce_types.script_attrib_t

  type textarea_elt = Eliom_duce_types.textarea_elt
  type textarea_attrib_t = Eliom_duce_types.textarea_attrib_t

  type input_elt = Eliom_duce_types.input_elt
  type input_attrib_t = Eliom_duce_types.input_attrib_t

  type select_elt = Eliom_duce_types.select_elt
  type select_content_elt = Eliom_duce_types.select_content_elt
  type select_content_elt_list = Eliom_duce_types.select_content_elt_list
  type select_attrib_t = Eliom_duce_types.select_attrib_t

  type button_elt = Eliom_duce_types.button_elt
  type button_content_elt = Eliom_duce_types.button_content_elt
  type button_content_elt_list = Eliom_duce_types.button_content_elt_list
  type button_attrib_t = Eliom_duce_types.button_attrib_t

  type option_elt = Eliom_duce_types.option_elt
  type option_elt_list = Eliom_duce_types.option_elt_list
  type optgroup_attrib_t = Eliom_duce_types.optgroup_attrib_t
  type option_attrib_t = Eliom_duce_types.option_attrib_t

  type input_type_t = Eliom_duce_types.input_type_t
  type raw_input_type_t = Eliom_duce_types.raw_input_type_t
  type button_type_t = Eliom_duce_types.button_type_t

  let hidden = {{ "hidden" }}
  let checkbox = {{ "checkbox" }}
  let radio = {{ "radio" }}
  let submit = {{ "submit" }}
  let file = {{ "file" }}
  let image = {{ "image" }}

  let buttonsubmit = {{ "submit" }}

  let uri_of_string x = x

  let empty_seq = {{ [] }}
  let cons_form a l = {{ [ a !l ] }}
  let map_option f l = {{ {: (List.map f l) :} }}
  let map_optgroup f a l = ((f a), {{ {: (List.map f l) :} }})

  let select_content_of_option a = (a :> select_content_elt)

  let make_pcdata s = str s

  let make_a ?(a={{ {} }}) ?href l : 'a a_elt =
    let href_attr = match href with
      | None -> {{ {} }}
      | Some v -> {{ { href=(str (Eliom_lazy.force v)) } }}
    in
    {{ <a (href_attr ++ a)> l }}

  let make_get_form ?(a={{ {} }}) ~(action : uri) elt1 elts : form_elt =
    {{ <form ({method="get"
                   action=(str action)}
              ++ a )>
       [ elt1 !elts ] }}

  let make_post_form ?(a={{ {} }}) ~(action : uri) ?id ?(inline = false) elt1 elts
      : form_elt =
    let id_attr = (match id with
    | None -> {{ {} }}
    | Some (i : string) -> {{ { id=(str i) } }})
    in
    let inline_attr = if inline then {{ { class="inline" } }} else {{ {} }} in
    {{ <form ({action=(str action)
               enctype="multipart/form-data"
               method="post"}
              ++ inline_attr
              ++ id_attr
              ++ a)>
       [ elt1
         !elts ]
     }}

  let make_hidden_field content =
    let c = match content with
      | None -> {{ [] }}
      | Some c -> {{ [ c ] }}
    in
    {{ <div class="eliom_nodisplay">c }}

  let make_div ~classe c =
    let classe = (List.fold_left (fun a b -> a^" "^b) "" classe) in
    {{ <div class=(str classe)> [ c ] }}

  let make_empty_form_content () = {{ <p> [] }} (**** à revoir !!!!! *)

  let remove_first = function {{ (hd,tl) }} -> (hd,tl) | {{ [] }} -> {{ <p>[] }}, {{ [] }}

  let make_input ?(a={{ {} }}) ?(checked=false) ~typ ?name ?src ?value () =
    let a2 = match value with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { value=(str v) } }}
    in
    let a3 = match name with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { name=(str v) } }}
    in
    let a4 = match src with
    | None -> {{ {} }}
    | Some (v : uri) -> {{ { src=(str v) } }}
    in
    let a5 = if checked then {{ { checked="checked" } }} else {{ {} }} in
    {{ <input ({type=typ} ++ a ++ a2 ++ a3 ++ a4 ++ a5)> [] }}

  let make_button ?(a={{ {} }}) ~button_type ?name ?value c =
    let a2 = match value with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { value=(str v) } }}
    in
    let a3 = match name with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { name=(str v) } }}
    in
    {{ <button ({type=button_type} ++ a ++ a2 ++ a3)> c }}

  let make_textarea ?(a={{ {} }}) ~name ?(value="") ~rows ~cols () =
    {{ <textarea ({ name=(str name)
                    rows={: string_of_int rows :}
                    cols={: string_of_int cols :}
                  } ++ a)> (str value) }}

  let make_select ?(a={{ {} }}) ~multiple ~name elt elts =
    let a2 = if multiple then {{ { multiple="multiple" } }} else {{ {} }} in
    {{ <select ({name=(str name)} ++ a2 ++ a)> [ elt !elts ] }}

  let make_option ?(a={{ {} }}) ~selected ?value c =
    let a2 = match value with
    | None -> {{ {} }}
    | Some (v : string) -> {{ { value=(str v) } }}
    in
    let a3 = if selected then {{ { selected="selected" } }} else {{ {} }} in
    {{ <option (a3 ++ a2 ++ a)> c }}

  let make_optgroup ?(a={{ {} }}) ~label elt elts =
    {{ <optgroup ({label=(str label)} ++ a)> [ elt !elts ] }}

  let make_css_link ?(a={{ {} }}) ~uri () =
    {{ <link ({href=(str uri)
            type="text/css"
            rel="stylesheet"}
            ++ a)> [] }}

  let make_js_script ?(a={{ {} }}) ~uri () =
    {{ <script ({type="text/javascript"
                 src=(str uri) } ++ a)> [] }}

end

module Xhtml_registration = Eliom_mkreg.MakeRegister(Xhtml_reg_base)

module Xhtml_forms = Eliom_mkforms.MakeForms(Xhtml_forms_base)

module Xhtml = struct
  include Xhtml_registration
  include Xhtml_forms
end

(****************************************************************************)
(****************************************************************************)

module TypedXML_content(TypedXML: XML_sigs_duce.TypedXML) = struct

  module Print = XML_print_duce.MakeTyped(TypedXML)

  type t = TypedXML.doc

  let get_etag_aux x =
    Some (Digest.to_hex (Digest.string x))

  let print (x: TypedXML.doc) =
    let b = Buffer.create 256 in
    Print.print
      ~advert:Ocsigen_pervasives.advert
      ~output:(Buffer.add_string b) x;
    Buffer.contents b

  let get_etag c =
    let x = print c in
    get_etag_aux x

  module S = Ocsigen_stream.StringStream

  let result_of_content c =
    let x = print c in
    let md5 = get_etag_aux x in
    let default_result = default_result () in
    Lwt.return
      {default_result with
         res_content_length = Some (Int64.of_int (String.length x));
         res_content_type = Some TypedXML.Info.content_type;
         res_etag = md5;
         res_headers= Http_headers.dyn_headers;
         res_stream = (S.make (S.put x), None)
      }

end

module TypedXML_partial_content(TypedXML: XML_sigs_duce.TypedXML) = struct

    module Print = XML_print_duce.MakeTyped(TypedXML)

    type t = TypedXML.elt list

    let get_etag_aux x =
      Some (Digest.to_hex (Digest.string x))

    let print (x: t) =
      let b = Buffer.create 256 in
      Print.print_list ~output:(Buffer.add_string b) x;
      Buffer.contents b

    let get_etag c =
      let x = print c in
      get_etag_aux x

    module S = Ocsigen_stream.StringStream

    let result_of_content c =
      let x = print c in
      let md5 = get_etag_aux x in
      let default_result = default_result () in
      Lwt.return
	{default_result with
           res_content_length = Some (Int64.of_int (String.length x));
           res_content_type = Some TypedXML.Info.content_type;
           res_etag = md5;
           res_headers= Http_headers.dyn_headers;
           res_stream = (S.make (S.put x), None)
	}

  end

module Make_Registration
  (TypedXML_content: sig
     type t
     val result_of_content: t -> Ocsigen_http_frame.result Lwt.t
   end) = struct

    module TypedXML_reg_base = struct

      type page = TypedXML_content.t
      type options = unit
      type return = Eliom_output.http_service
      type result = Ocsigen_http_frame.result

      let result_of_http_result x = x

      let send_appl_content = Eliom_services.XNever

      let send ?options ?charset ?code
          ?content_type ?headers content =
        TypedXML_content.result_of_content content >>= fun r ->
        Lwt.return
          {r with
             res_code= code_of_code_option code;
             res_charset= Some "utf-8"
              (* For Eliom_duce, we impose utf-8 *);
             res_content_type= (match content_type with
                                  | None -> r.res_content_type
                                  | _ -> content_type
                               );
             res_headers= (match headers with
                             | None -> r.res_headers
                             | Some headers ->
                                 Http_headers.with_defaults
                                   headers r.res_headers
                          );
          }

    end

    include Eliom_mkreg.MakeRegister(TypedXML_reg_base)

  end

module Make_TypedXML_Registration(TypedXML: XML_sigs_duce.TypedXML) =
  Make_Registration(TypedXML_content(TypedXML))

module Make_Partial_TypedXML_Registration(TypedXML: XML_sigs_duce.TypedXML) =
  Make_Registration(TypedXML_partial_content(TypedXML))

module Blocks = struct
  include Make_Partial_TypedXML_Registration(XHTML_duce.M)
  include Xhtml_forms
end

(* module Xml = SubXhtml(struct *)
                        (* type content = Ocamlduce.Load.anyxml *)
                        (* let print f x = XML_print_duce.print ~output:f x *)
                      (* end) *)

(* module Xmllist = SubXhtml(struct *)
                            (* type content = Ocamlduce.Load.anyxml list *)
                            (* let print f (x : content) = *)
                              (* List.iter *)
                                (* (XML_print_duce.print ~output:f) *)
                                (* x *)
                          (* end) *)
