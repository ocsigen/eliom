
{shared{
  open Eliom_content
  open Eliom_lib
}}


let simple_dom_react = Eliom_testsuite_base.test
    ~title: "Client dom react"
    ~path:["React";"client";"simple"]
    ~description:Html5.F.([pcdata "a simple example of dom react"])
    (fun () ->
       let parent1 = Html5.D.div [] in
       let update = {(unit -> unit) ref{ref (fun () -> ())}} in
       let input = Html5.D.input ~input_type:`Text ~a:[Html5.D.a_onkeyup {{ fun _ -> !(%update) () }}] () in
       let _ = {unit{
         let s,set_s = React.S.create "" in
         let i,set_i = React.S.create 0 in
         let valid = React.S.map (fun s -> try set_i (int_of_string s);true with _ -> false) s in
         let v_succ = React.S.map succ i in
         let v_double = React.S.map (( * ) 2) i in
         let v_minus = React.S.map (fun x -> - x) i in
         let r_int s = Html5.R.pcdata (React.S.map string_of_int s) in
         let style = React.S.map (fun x -> if x then "color:green;" else "color:red;") valid in
         let dom = Html5.(F.div ~a:[R.a_style style] [
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
           ]) in
         Html5.Manip.appendChild (%parent1) %input;
         Html5.Manip.appendChild (%parent1) dom;
         (%update):=(fun () ->
             let s = Js.to_string ((Html5.To_dom.of_input %input)##value)
             in set_s s)
       }} in
       Lwt.return [ parent1 ]
    )

let tests = [
  "Client : Simple Dom React", [
    simple_dom_react
  ]]
