include ../Makefile.config

NAME=$(OCSIGENNAME)

EXTENSIONSFILES= ocsigenmod.ml ocsigen.ml ocsigenboxes.ml staticmod.ml

FILES= error_pages.ml preemptive.ml extensions.ml parseconfig.ml server.ml \
       $(EXTENSIONSFILES)

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

ifeq "$(OCAMLDUCE)" "YES"
DUCEFILES= xhtml1_strict.ml ocsigenduce.ml
# rss2.ml ocsigenrss.ml

  ifeq "$(BYTECODE)" "YES"
  DUCEMODBYTE=ocsigenduce.cma
# ocsigenrss.cma
  DUCEINSTALLBYTE=ocsigenduce.cma
# ocsigenrss.cma
  else
  DUCEMODBYTE=
  DUCEINSTALLBYTE=
  endif

  ifeq "$(NATIVECODE)" "YES"
  DUCEMODOPT=ocsigenduce.cmxs
# ocsigenrss.cmxs
  DUCEINSTALLOPT=ocsigenduce.cmxs
# ocsigenrss.cmxs
  else
  DUCEMODOPT=
  DUCEINSTALLOPT=
  endif

DUCEINSTALL=$(DUCEINSTALLBYTE) $(DUCEINSTALLOPT) \
	ocsigenduce.cmi xhtml1_strict.cmi
# ocsigenrss.cmi rss2.cmi
OCAMLFIND=$(OCAMLDUCEFIND)
else
DUCEFILES=
DUCEMODBYTE=
DUCEMODOPT=
DUCEINSTALL=
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


byte: $(OBJS) $(NAME) ocsigenmod.cma staticmod.cmo $(DUCEMODBYTE)
#	cp -f ocsigenmod.cma ocsigen.cmi staticmod.cmo preemptive.cmi extensions.cmi $(DUCEINSTALLBYTE) ../lib
#	cp -f ocsigenboxes.cmi ../lib
#	cp -f $(NAME) ../bin

opt: $(OBJSX) $(NAME).opt ocsigenmod.cmxs staticmod.cmxs $(DUCEMODOPT)
#	cp -f ocsigenmod.cmxs ocsigen.cmi staticmod.cmxs preemptive.cmi extensions.cmi $(DUCEINSTALLOPT) ../lib
#	cp -f ocsigenboxes.cmi ../lib
#	cp -f $(NAME).opt ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	$(CAMLC) -o $(NAME) -linkall $(CMAO)

$(NAME).opt: $(OBJSOPT)
	$(CAMLOPT) -o $(NAME).opt -linkall $(CMXXA)

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenmod.cmxs: ocsigenmod.cmx ocsigen.cmx ocsigenboxes.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenmod.cmxs ocsigenmod.cmx ocsigen.cmx \
	ocsigenboxes.cmx

staticmod.cmxs: staticmod.cmx
	$(CAMLOPT) -shared -linkall -o staticmod.cmxs staticmod.cmx

ocsigenduce.cma: xhtml1_strict.cmo ocsigenduce.cmo
	$(CAMLC) -a -o ocsigenduce.cma xhtml1_strict.cmo ocsigenduce.cmo 

ocsigenduce.cmxs: xhtml1_strict.cmx ocsigenduce.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenduce.cmxs xhtml1_strict.cmx ocsigenduce.cmx 

ocsigenrss.cma: rss2.cmo ocsigenrss.cmo
	$(CAMLC) -a -o ocsigenrss.cma $^

ocsigenrss.cmxs: rss2.cmx ocsigenrss.cmx
	$(CAMLOPT) -shared -linkall -o ocsigenrss.cmxs $^

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	$(CAMLDOC) -d doc -html ocsigen.mli extensions.mli

.PHONY: clean
clean:
	-rm -f *.cm[aiox] *.cmxa *.cmxs *~ doc/* $(NAME) *.annot

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


