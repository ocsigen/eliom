open XHTML.M
open Ocsigen

let menu ?(classe=[]) l current current_url =
  let rec aux = function
      [] -> []
    | [(url,text)] -> 
	let classe = ["last"] in
	if url == current 
	then [li ~a:[a_class ("current"::classe)] text]
	else [li ~a:[a_class classe] [a text current_url url]]
    | (url,text)::l -> 
	(if url == current 
	then  (li ~a:[a_class ["current"]] text)
	else (li [a text current_url url]))::(aux l)
  in match l with
    [] -> << <!-- empty menu --> >>
  | [(url,text)] ->
      ul ~a:[a_class ("menu"::classe)] 
	(let liclasse = ["first";"last"] in
	if url == current 
	then (li ~a:[a_class ("current"::liclasse)] text) 
	else (li ~a:[a_class liclasse] [a text current_url url])) []
  | (url,text)::l -> 
      ul ~a:[a_class ("menu"::classe)]
	(let liclasse = ["first"] in
	if url == current 
	then (li ~a:[a_class ("current"::liclasse)] text)
	else (li ~a:[a_class liclasse] [a text current_url url])) (aux l)
