include ../Makefile.config

NAME=$(OCSIGENNAME)

FILES= error_pages.ml preemptive.ml extensions.ml parseconfig.ml server.ml

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(PROFILING)" "YES"
CAMLCNAME= ocamlcp -p a
else
CAMLCNAME= ocamlc
endif

ifeq "$(BYTECODE)" "YES"
INSTALLBYTE=installbyte
else
INSTALLBYTE=
endif

ifeq "$(NATIVECODE)" "YES"
INSTALLOPT=installopt
else
INSTALLOPT=
endif


ifeq "$(NEWOCAMLNET)" "YES"
NETSYS=netsys,
NETSYSCMA=netsys.cma
NETSYSCMXA=netsys.cmxa
else
NETSYS=
NETSYSCMA=
NETSYSCMXA=
endif

INSTALL = install
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB1 = -thread -package netstring,$(NETSYS)ssl $(LIBDIRS) -I +camlp4
PP = 
PP2 = 
# -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc"


# cma and cmo to be linked in the server:

ifeq ($(OCAMLVERSION),OLD)
  CMAOCAMLP4= gramlib.cma
  CMXXACAMLP4= gramlib.cmxa
  LIB=$(LIB1)
else
  CMAOCAMLP4= camlp4lib.cma Camlp4OCamlRevisedParser.cmo Camlp4OCamlParser.cmo
  CMXXACAMLP4= camlp4lib.cmxa Camlp4OCamlRevisedParser.cmx Camlp4OCamlParser.cmx
  LIB=$(LIB1) -I +camlp4/Camlp4Parsers
endif

CMAO = nums.cma dynlink.cma unix.cma \
       ssl.cma pcre.cma $(NETSYSCMA) netstring.cma \
       threads.cma \
       ocsimisc.cmo \
       $(CMAOCAMLP4) \
       xmllexer.cmo simplexmlparser.cmo \
       ocsiconfig.cmo messages.cmo \
       lwt.cma preemptive.cmo \
       extensions.cmo parseconfig.cmo \
       ocsistream.cmo multipart.cmo \
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtml.cma predefined_senders.cmo error_pages.cmo \
       server.cmo

OBJS = $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo)

CMXXATEMP=$(CMAO:.cmo=.cmx)
CMXXA=$(CMXXATEMP:.cma=.cmxa)

OBJSOPT = $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx)


byte: $(OBJS) $(NAME)

opt: $(OBJSX) $(NAME).opt


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	$(CAMLC) -o $(NAME) -linkall $(CMAO)

$(NAME).opt: $(OBJSOPT)
	$(CAMLOPT) -o $(NAME).opt -linkall $(CMXXA)

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html eliom.mli ocsigen.mli extensions.mli

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/* $(NAME) $(NAME).opt *.annot

.PHONY: install
install: $(INSTALLBYTE) $(INSTALLOPT)

installbyte:
	@echo "Installing bytecode server"
	mkdir -p $(PREFIX)/$(BINDIR)
	$(INSTALL) -m 755 $(NAME) $(PREFIX)/$(BINDIR)/

installopt:
	@echo "Installing native server"
	mkdir -p $(PREFIX)/$(BINDIR)
	$(INSTALL) -m 755 $(NAME).opt $(PREFIX)/$(BINDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/$(BINDIR)/$(NAME)
	rm -f $(PREFIX)/$(BINDIR)/$(NAME).opt

depend:
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend


FORCE:

include .depend


