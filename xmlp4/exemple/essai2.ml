open Xhtmlpp

let arbre = << <html> <head> 
			 	<title>Essai2</title> 
			 </head> 
			<body>
				<div>
					je suis dans <b><i>div</i></b> avec div en gras 
				</div>
				<h1>
					je suis dans h1
				</h1>
			</body> 
		</html> >>;;
print_string (xh_print arbre);;


