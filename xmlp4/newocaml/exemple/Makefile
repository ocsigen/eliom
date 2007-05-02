NAME = essai
NAME2 = essai2

FILES = essai2.ml essai.ml

CAMLC = ocamlc $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDEP = ocamldep
CAMLP4 = camlp4
LIB = -I `$(CAMLP4) -where` -I ..
PP = -pp "camlp4of ../xmllexer.cmo ../xhtmlparser.cmo ../xhtmlsyntax.cmo -loc loc"

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS) $(NAME) $(NAME2)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

$(NAME): $(NAME).ml
	$(CAMLC) -o $(NAME) ../../ohl-xhtml/xHTML.cmo  ../xhtmltypes.cmo  $(NAME).cmo

$(NAME2): $(NAME2).ml
	$(CAMLC) -o $(NAME2)  ../../ohl-xhtml/xHTML.cmo   ../xhtmltypes.cmo  $(NAME2).cmo

.ml.cmo:
	$(CAMLC) $(PP)  ../../ohl-xhtml/xHTML.cmo   ../xhtmltypes.cmo  -c $<
.mli.cmi:
	$(CAMLC)   ../../ohl-xhtml/xHTML.cmo   ../xhtmltypes.cmo  -c $<
.ml.cmx:
	$(CAMLOPT) $(PP)   ../../ohl-xhtml/xHTML.cmo   ../xhtmltypes.cmo  -c $<

clean:
	-rm -f *.cm[iox] *~ essai essai2
