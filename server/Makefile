include ../Makefile.config

NAME=$(OCSIGENNAME)

FILES= error_pages.ml preemptive.ml extensions.ml parseconfig.ml \
       server.ml

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

ifeq "$(PROFILING)" "YES"
CAMLCNAME2= $(CAMLCPNAME)
else
CAMLCNAME2= $(CAMLCNAME)
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
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
LIB = -thread -package netstring,$(NETSYS)ssl,pcre,unix,dynlink,threads,camlp4.gramlib,num $(LIBDIRS)
PP = 
PP2 = 

# cma and cmo to be linked in the server:

#ifeq ($(OCAMLVERSION),OLD)
#  CMAOCAMLP4= gramlib.cma
#  CMXXACAMLP4= gramlib.cmxa
#  LIB=$(LIB1)
#else
#  CMAOCAMLP4= camlp4lib.cma Camlp4OCamlRevisedParser.cmo Camlp4OCamlParser.cmo
#  CMXXACAMLP4= camlp4lib.cmxa Camlp4OCamlRevisedParser.cmx Camlp4OCamlParser.cmx
#  LIB=$(LIB1) -I +camlp4/Camlp4Parsers
#endif

CMAO = ../baselib/ocsimisc.cmo \
       ../xmlp4/xmllexer.cmo ../xmlp4/simplexmlparser.cmo \
       ../baselib/ocsiconfig.cmo ../baselib/messages.cmo \
       ../lwt/lwt.cma \
       ../http/ocsistream.cmo ../http/multipart.cmo \
       ../http/http_headers.cmo \
       ../http/http_frame.cmo ../http/ocsiheaders.cmo \
       ../http/http_lexer.cmo ../http/http_parser.cmo \
       ../http/framepp.cmo ../http/http_com.cmo \
       ../xmlp4/xhtml.cma ../http/predefined_senders.cmo

OBJS = nagle.o $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo)

CMXXATEMP=$(CMAO:.cmo=.cmx)
CMXXA=$(CMXXATEMP:.cma=.cmxa)

OBJSOPT = $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx)


byte: $(OBJS) $(NAME)

opt: $(OBJSX) $(NAME).opt


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(CMAO) $(OBJS)
	$(CAMLC) -o $(NAME) -linkpkg -linkall $(CMAO) $(OBJS)

$(NAME).opt: $(CMXXA) $(OBJSOPT)
	$(CAMLOPT) -o $(NAME).opt -linkpkg -linkall $(CMXXA) $(OBJSOPT)

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html eliom.mli ocsigen.mli extensions.mli


nagle.o: nagle.c
	$(CC) $(CAMLINCLUDE) -c $<

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/* $(NAME) $(NAME).opt *.annot

.PHONY: install
install: $(INSTALLBYTE) $(INSTALLOPT)

installbyte:
	@echo "Installing bytecode server"
	mkdir -p $(TEMPROOT)/$(BINDIR)
	$(INSTALL) -m 755 $(NAME) $(TEMPROOT)/$(BINDIR)/

installopt:
	@echo "Installing native server"
	mkdir -p $(TEMPROOT)/$(BINDIR)
	[ -f $(NAME).opt ] && $(INSTALL) -m 755 $(NAME).opt $(TEMPROOT)/$(BINDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(TEMPROOT)/$(BINDIR)/$(NAME)
	rm -f $(TEMPROOT)/$(BINDIR)/$(NAME).opt

depend:
	$(CAMLDEP) $(LIBDIRS) $(PP2) $(FILES:.ml=.mli) $(FILES) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend


FORCE:

-include .depend


