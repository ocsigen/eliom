
{shared{
  open Eliom_content
  open Eliom_lib
}}


let simple_dom_react = Eliom_testsuite_base.test
    ~title: "Client dom react"
    ~path:["React";"client";"simple"]
    ~description:Html5.F.([pcdata "a simple example of dom react"])
    (fun () ->
       let signal = {(string React.S.t * (string -> unit)){
           let s,set =  React.S.create "" in
           s, (fun s -> set s)}} in
       let input = Html5.D.input ~input_type:`Text ~a:[Html5.D.a_onkeyup {{ fun e ->
           let i = Dom.eventTarget e in
           let i = Js.Unsafe.coerce i in
           (snd %signal)(Js.to_string (i##value))
         }}] () in
       let dom = {{
         let s,_ = %signal in
         let i,set_i = React.S.create 0 in
         let valid = React.S.map (fun s -> try set_i (int_of_string s);true with _ -> false) s in
         let v_succ = React.S.map succ i in
         let v_double = React.S.map (( * ) 2) i in
         let v_minus = React.S.map (fun x -> - x) i in
         let r_int s = Html5.R.pcdata (React.S.map string_of_int s) in
         let style = React.S.map (fun x -> if x then "color:green;" else "color:red;") valid in
         Html5.(F.div ~a:[R.a_style style] [
             F.div [ F.pcdata "textarea : " ; R.textarea ~a:[F.a_maxlength 10; F.a_readonly `ReadOnly ; F.a_style "vertical-align: top"] (React.S.map F.pcdata s)];
             F.div [ F.pcdata "original : "; r_int i];
             F.div [ F.pcdata "succ : "; r_int v_succ];
             F.div [ F.pcdata "double : "; r_int v_double];
             F.div [ F.pcdata "minus : "; r_int v_minus];
             R.node (React.S.map (fun i ->
                 let f = float_of_int i in
                 try
                   let sq = int_of_float (sqrt f) in
                   let sq =
                     if sq * sq = i
                     then sq
                     else raise Not_found in
                   F.div [F.pcdata "sqrt : "; F.pcdata (string_of_int sq)]
                 with _ ->
                   F.div [F.pcdata "not natural sqrt"]) i)
           ])
       }} in
       Lwt.return [ Html5.F.div [input;Html5.C.node dom] ]
    )

let tests = [
  "Client : Simple Dom React", [
    simple_dom_react
  ]]
