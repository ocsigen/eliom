include ../Makefile.config

NAME = ocsigen

FILES= config.ml messages.ml error_pages.ml ocsigen.ml ocsigenboxes.ml server.ml

INSTALL = install -c
CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
LIB = -package netstring,dbi.postgresql -I ../lib -I +camlp4
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xhtmlparser.cmo ../lib/xhtmlsyntax.cmo"

CMAO = nums.cma dynlink.cma unix.cma pcre.cma netstring.cma lwt.cma\
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtml.cma sender_helpers.cmo\
       error_pages.cmo\
       postgresql.cma dbi.cma dbi_postgresql.cmo\
       gramlib.cma xmllexer.cmo simplexmlparser.cmo
# avant dernière ligne pas nécessaire pour server mais je n'arrive pas à le lier ailleurs...

OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS) $(NAME) doc
#	cp *.cmo *.cmi ../lib
	cp ocsigen.cmo ocsigen.cmi ../lib
	cp messages.cmi config.cmi ../lib
	cp ocsigenboxes.cmi ../lib
	cp $(NAME) ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	$(CAMLC) -o $(NAME) $(CMAO) $(OBJS)

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html ocsigen.mli

.PHONY: clean
clean:
	-rm -f *.cm[iox] *~ $(NAME)

.PHONY: install
install:
	@echo "Installing binary"
	mkdir -p $(BINDIR)
	$(INSTALL) -m755 $(NAME) $(BINDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(BINDIR)/$(NAME)

depend:
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


