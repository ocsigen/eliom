NAME = krokoserver

FILES=  messages.ml error_pages.ml kroko.ml krokoserver.ml

CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package netstring,dbi.postgresql -I ../lib
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xmlparser.cmo"

CMAO = nums.cma dynlink.cma unix.cma pcre.cma netstring.cma lwt.cma\
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtmlpp.cmo sender_helpers.cmo\
       error_pages.cmo\
       postgresql.cma dbi.cma dbi_postgresql.cmo
# dernière ligne pas nécessaire pour server mais je n'arrive pas à le lier ailleurs...


OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS) $(NAME) doc
#	cp *.cmo *.cmi ../lib
	cp kroko.cmo kroko.cmi ../lib
	cp messages.cmi ../lib
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
	$(CAMLDOC) -d doc -html kroko.mli

clean:
	-rm -f *.cm[iox] *~ $(NAME)

depend:
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


