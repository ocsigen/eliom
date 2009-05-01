include ../Makefile.config

NAME=$(OCSIGENNAME)

FILES= ocsigen_extensions.ml ocsigen_parseconfig.ml ocsigen_http_client.ml \
	../extensions/ocsigen_LocalFiles.ml ocsigen_server.ml
EXE= server_main.ml


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

ifeq "$(NATDYNLINK)" "YES"
DYNLINK=dynlink,
else
DYNLINK=
endif

ifeq ($(OCAMLVERSION),OLD)
#  CMAOCAMLP4= gramlib.cma
#  CMXXACAMLP4= gramlib.cmxa
  CMAOCAMLP4=
  CMXXACAMLP4=
#  LIB=$(LIB1)
  CAMLP4PACKAGES=,camlp4.gramlib
else
  CAMLP4PACKAGES= 
  CMAOCAMLP4= camlp4lib.cma Camlp4OCamlRevisedParser.cmo Camlp4OCamlParser.cmo
  CMXXACAMLP4= camlp4lib.cmxa Camlp4OCamlRevisedParser.cmx Camlp4OCamlParser.cmx
  LIBDIRS := $(LIBDIRS) -I +camlp4 -I +camlp4/Camlp4Parsers
endif

INSTALL = install
CAMLC = $(OCAMLFIND) $(CAMLCNAME2) $(DBG) -package dynlink $(LIB)
CAMLMKTOP = $(OCAMLFIND) $(CAMLMKTOPNAME) $(LIB)
CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep $(LIBDIRS)
LIB = -thread -package lwt.ssl,lwt.extra,netstring,$(NETSYS)ssl,pcre,unix,$(DYNLINK)threads$(CAMLP4PACKAGES),findlib $(LIBDIRS)
# also modify builtin_package in ocsigen_config.ml if you modify the former line!
PP = 
PP2 = 

ifdef DONOTPARSECOMMANDLINE
PARSECOMMANDLINECMA=../baselib/donotparsecommandline.cma
else
PARSECOMMANDLINECMA=../baselib/parsecommandline.cma
endif
PARSECOMMANDLINECMXA=$(PARSECOMMANDLINECMA:.cma=.cmxa)

# cma and cmo to be linked in the server:

CMOS = ../baselib/ocsigen_lib.cmo \
       ../xmlp4/xmllexer.cmo ../xmlp4/simplexmlparser.cmo \
       ../baselib/ocsigen_config.cmo \
       ../baselib/ocsigen_commandline.cmo \
	../baselib/ocsigen_messages.cmo \
       ../baselib/dynlink_wrapper.cmo ../baselib/ocsigen_loader.cmo \
       ../baselib/ocsigen_stream.cmo ../http/multipart.cmo \
       ../http/http_headers.cmo \
       ../http/ocsigen_http_frame.cmo ../http/ocsigen_headers.cmo \
       ../http/ocsigen_range.cmo \
       ../http/http_lexer.cmo ../http/http_parser.cmo \
       ../http/framepp.cmo ../http/ocsigen_http_com.cmo \
       ../xmlp4/xhtmlpretty_streams.cmo ../xmlp4/xhtmlcompact_streams.cmo \
       ../http/ocsigen_charset_mime.cmo ../http/ocsigen_senders.cmo

CMAO = ../xmlp4/xhtml.cma ../xmlp4/xhtmlpretty.cma $(NAME).cma

STUBS_OBJS = stubs.o

OBJS = $(FILES:.ml=.cmo) $(DUCEFILES:.ml=.cmo)
OBJSEXE = $(EXE:.ml=.cmo)

CMXSATEMP=$(CMOS:.cmo=.cmx)
CMXSA=$(CMXSATEMP:.cma=.cmxa)
CMXXATEMP=$(CMAO:.cmo=.cmx)
CMXXA=$(CMXXATEMP:.cma=.cmxa)

OBJSOPT = $(FILES:.ml=.cmx) $(DUCEFILES:.ml=.cmx)
OBJSEXEOPT = $(EXE:.ml=.cmx)


byte: $(OBJS) $(OBJSEXE) $(NAME) $(NAME)top

opt: $(OBJSOPT) $(OBJSEXEOPT) $(NAME).opt


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(CMAO) $(OBJSEXE) $(PARSECOMMANDLINECMO)
	$(CAMLC) -o $(NAME) -linkpkg -linkall $(PARSECOMMANDLINECMA) $(CMAOCAMLP4) $(CMAO) $(OBJSEXE)

$(NAME).opt: $(CMXXA) $(OBJSEXEOPT) $(PARSECOMMANDLINECMX)
	$(CAMLOPT) -o $(NAME).opt -linkpkg -linkall $(PARSECOMMANDLINECMXA) $(CMXXACAMLP4) $(CMXXA) $(OBJSEXEOPT)

$(NAME).cma: $(CMOS) $(OBJS) $(STUBS_OBJS)
	$(CAMLMKLIB) -o $(NAME) -oc $(NAME) -linkall $(CMOS) $(OBJS) $(STUBS_OBJS)

$(NAME).cmxa: $(CMXSA) $(OBJSOPT) $(STUBS_OBJS)
	$(CAMLMKLIB) -o $(NAME) -oc $(NAME) -linkall $(CMXSA) $(OBJSOPT) $(STUBS_OBJS)

$(NAME)top: $(CMAO) $(PARSECOMMANDLINECMO)
	$(CAMLMKTOP) -o $(NAME)top -linkpkg -linkall $(PARSECOMMANDLINECMA) $(CMAOCAMLP4) $(CMAO)

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

#doc:
#	$(CAMLDOC) -d doc -html eliom.mli ocsigen.mli ocsigen_extensions.mli


stubs.o: stubs.c
	$(CAMLC) $(CAMLINCLUDE) -c $<

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *.o *.a *~ doc/* $(NAME) $(NAME).opt $(NAME)top *.annot *.so

.PHONY: install
install: install_so $(INSTALLBYTE) $(INSTALLOPT)

install_so:
	@echo "Installing shared stubs"
	mkdir -p $(TEMPROOT)$(STUBDIR)
	$(INSTALL) -m 755 dll$(NAME).so $(TEMPROOT)$(STUBDIR)/

installbyte:
	@echo "Installing bytecode server"
	mkdir -p $(TEMPROOT)$(BINDIR)
	$(INSTALL) -m 755 $(NAME) $(TEMPROOT)$(BINDIR)/

installopt:
	@echo "Installing native server"
	mkdir -p $(TEMPROOT)$(BINDIR)
	[ -f $(NAME).opt ] && $(INSTALL) -m 755 $(NAME).opt $(TEMPROOT)$(BINDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(TEMPROOT)$(BINDIR)/$(NAME)
	rm -f $(TEMPROOT)$(BINDIR)/$(NAME).opt
	rm -f $(TEMPROOT)$(STUBDIR)/dll$(NAME).so

depend:
	$(CAMLDEP) $(LIBDIRS) $(PP2) $(FILES:.ml=.mli) $(EXE:.ml=.mli) $(FILES) $(EXE) $(DUCEFILES:.ml=.mli) $(DUCEFILES) > .depend


FORCE:

-include .depend


