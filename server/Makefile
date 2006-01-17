include ../Makefile.config

NAME = ocsigen

FILES= error_pages.ml ocsigen.ml ocsigenboxes.ml server.ml

INSTALL = install
CAMLC = ocamlfind ocamlc -g $(LIB)
CAMLOPT = ocamlfind ocamlopt $(LIB)
CAMLDOC = ocamlfind ocamldoc $(LIB)
CAMLDEP = ocamlfind ocamldep
ifeq "$(OCSIMORE)" "YES"
OCSIMOREPCKG = dbi.postgresql,
else
OCSIMOREPCKG =
endif
LIB = -package $(OCSIMOREPCKG)netstring -I ../lib -I +camlp4
PP = -pp "camlp4o ../lib/xhtmlsyntax.cma"

CMAO = nums.cma dynlink.cma unix.cma pcre.cma netstring.cma lwt.cma\
       gramlib.cma xmllexer.cmo simplexmlparser.cmo\
       ocsiconfig.cmo messages.cmo\
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtml.cma sender_helpers.cmo\
       error_pages.cmo

OCSIMORECMAO = postgresql.cma dbi.cma dbi_postgresql.cmo
# Last line is only for ocsimore. But I don't know how to link them elsewhere

OBJS = $(FILES:.ml=.cmo)

all: depend $(OBJS) $(NAME) doc
#	cp *.cmo *.cmi ../lib
	cp ocsigen.cmo ocsigen.cmi ../lib
#	cp config.cmi ../lib
	cp ocsigenboxes.cmi ../lib
	cp $(NAME) ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Ocsigen server with ocsimore support";\
	$(CAMLC) -o $(NAME) $(CMAO) $(OCSIMORECMAO) $(OBJS);\
	else echo "Ocsigen server without ocsimore support";\
	$(CAMLC) -o $(NAME) $(CMAO) $(OBJS);\
	fi

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


