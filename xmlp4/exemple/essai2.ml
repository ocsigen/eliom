open Xhtmlpp

let a = <:xmllist< <em> hello </em>  >>

let arbre = << <html>   <head> 
			 	<title>Essai2</title> 
			 </head> 
			<body>
				<div>
					je suis   dans <b><i>div</i></b> avec div en gras 
				</div>
				<h1>  <!-- commentaire --> a$ je suis dans h1  </h1>
			</body>

			<!-- qq < <!-- commentaires -- <!- imbriqués -> --> ss -->
		</html> >>;;


print_string (xh_print arbre);;


let a = << <html id="lkj"><head/><body> 
<p id="lkj"> <em id="lkj"> ml</em> </p> </body></html> >> in

print_string (xh_print a);;
