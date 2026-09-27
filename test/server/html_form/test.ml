(* Tests of Html_form, the forms of a page, on pages written by hand *)

open Eliom_test_server

let form =
  Alcotest.testable
    (fun fmt {Html_form.meth; action; fields} ->
       Format.fprintf fmt "%s %s [%s]"
         (match meth with `GET -> "GET" | `POST -> "POST")
         action
         (String.concat "; " (List.map (fun (n, v) -> n ^ "=" ^ v) fields)))
    ( = )

let check msg expected ~url page =
  Alcotest.(check (list form)) msg expected (Html_form.forms ~url page)

let test_fields () =
  check "submitted fields"
    [ { Html_form.meth = `POST
      ; action = "/target"
      ; fields =
          [ "text", "a&b \"c\""
          ; "hidden", "h"
          ; "checked", "on"
          ; "radio", "y"
          ; "area", "one\ntwo <&>"
          ; "select", "b"
          ; "first", "1"
          ; "multiple", "m1"
          ; "multiple", "m3" ] } ]
    ~url:"/page"
    {|<form method="post" action="/target">
       <input type="text" name="text" value="a&amp;b &quot;c&quot;"/>
       <input type="hidden" name="hidden" value="h"/>
       <input type="checkbox" name="checked" checked="checked"/>
       <input type="checkbox" name="unchecked" value="u"/>
       <input type="radio" name="radio" value="x"/>
       <input type="radio" name="radio" value="y" checked="checked"/>
       <input type="submit" name="button" value="b"/>
       <input type="text" value="no name"/>
       <textarea name="area">one
two &lt;&amp;&gt;</textarea>
       <select name="select"><option value="a">A</option>
         <option value="b" selected="selected">B</option></select>
       <select name="first"><option>1</option><option>2</option></select>
       <select name="multiple" multiple="multiple">
         <option value="m1" selected="selected">1</option>
         <option value="m2">2</option>
         <option value="m3" selected="selected">3</option></select>
     </form>|}

let test_page () =
  check "forms of a page"
    [ {Html_form.meth = `GET; action = "/dir/target?x=1"; fields = ["a", "1"]}
    ; {Html_form.meth = `GET; action = "/dir/page?p=q"; fields = []} ]
    ~url:"/dir/page?p=q"
    {|<!DOCTYPE html><html><head><!-- <form> -->
       <script>if (a < b) document.write("<form>")</script></head><body>
       <input type="text" name="outside" value="o"/>
       <form action="target?x=1"><input name="a" value="1"/></form>
       <form></form></body></html>|}

let test_set () =
  let f =
    { Html_form.meth = `GET
    ; action = "/target"
    ; fields = ["a", "1"; "b", "2"; "a", "3"] }
  in
  Alcotest.check form "replaced"
    {f with fields = ["a", "x"; "b", "2"]}
    (Html_form.set "a" "x" f);
  Alcotest.check form "added"
    {f with fields = f.fields @ ["c", "y"]}
    (Html_form.set "c" "y" f)

let () =
  Alcotest.run "html-form"
    [ ( "forms"
      , [ Alcotest.test_case "fields" `Quick test_fields
        ; Alcotest.test_case "page" `Quick test_page
        ; Alcotest.test_case "set" `Quick test_set ] ) ]
