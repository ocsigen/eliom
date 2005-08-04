NAME = essai
NAME2 = essai2

FILES = essai2.ml essai.ml

CAMLC = ocamlc $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDEP = ocamldep
CAMLP4 = camlp4
LIB = -I `$(CAMLP4) -where` -I ..
PP = -pp "camlp4o ../xmllexer.cmo ../xmlparser.cmo"

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS) $(NAME) $(NAME2)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

$(NAME): $(NAME).ml
	$(CAMLC) -o $(NAME) $(NAME).cmo

$(NAME2): $(NAME2).ml
	$(CAMLC) -o $(NAME2) ../xhtmlpp.cmo $(NAME2).cmo

.ml.cmo:
	$(CAMLC) $(PP) ../xhtmlpp.cmo -c $<
.mli.cmi:
	$(CAMLC) ../xhtmlpp.cmo -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) ../xhtmlpp.cmo -c $<

clean:
	-rm -f *.cm[iox] *~ essai
