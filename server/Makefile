NAME = server

FILES=  messages.ml error_pages.ml omlet.ml server.ml

CAMLC = ocamlc -g $(LIB)
CAMLOPT = ocamlopt $(LIB)
CAMLDOC = ocamldoc $(LIB)
CAMLDEP = ocamldep
LIB = -I ../lib -I +netstring -I +pcre
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xmlparser.cmo"

CMAO = nums.cma dynlink.cma unix.cma pcre.cma netstring.cma lwt.cma\
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtmlpp.cmo sender_helpers.cmo\
       error_pages.cmo
#       postgres.cma dbi.cma dbi_postgres.cmo\

OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS) $(NAME) doc
	cp *.cmo *.cmi ../lib
	cp $(NAME) ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc

$(NAME): $(OBJS)
	$(CAMLC) -o $(NAME) $(CMAO) $(OBJS)

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	$(CAMLDOC) -d doc -html omlet.mli

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


