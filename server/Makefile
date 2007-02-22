include ../Makefile.config

NAME=$(OCSIGENNAME)

FILES= error_pages.ml preemptive.ml extensions.ml parseconfig.ml server.ml

EXTENSIONS= ocsigenmod.ml ocsigen.ml ocsigenboxes.ml staticmod.ml

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
DUCEMOD=ocsigenduce.cma
DUCEINSTALL=ocsigenduce.cma ocsigenduce.cmi xhtml1_strict.cmi 
else
DUCEMOD=
DUCEINSTALL=
endif

ifeq "$(NEWOCAMLNET)" "YES"
NETSYS=netsys,
NETSYSCMA=netsys.cma
else
NETSYS=
NETSYSCMA=
endif

INSTALL = install
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -thread -package netstring,$(NETSYS)ssl -I ../lib -I +camlp4
PP = 
PP2 = -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc"

CMAO = nums.cma dynlink.cma unix.cma \
       ssl.cma pcre.cma $(NETSYSCMA) netstring.cma \
       threads.cma \
       ocsimisc.cmo \
       gramlib.cma xmllexer.cmo simplexmlparser.cmo \
       ocsiconfig.cmo messages.cmo \
       lwt.cma preemptive.cmo \
       extensions.cmo parseconfig.cmo \
       ocsistream.cmo multipart.cmo \
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtml.cma predefined_senders.cmo\
       error_pages.cmo

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS) $(NAME) ocsigenmod.cma staticmod.cmo $(DUCEMOD)
#	cp *.cmo *.cmi ../lib
	cp ocsigenmod.cma ocsigen.cmi staticmod.cmo preemptive.cmi extensions.cmi $(DUCEINSTALL) ../lib
#	cp config.cmi ../lib
	cp ocsigenboxes.cmi ../lib
	cp $(NAME) ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	$(CAMLC) -o $(NAME) -linkall $(CMAO) $(OBJS)

ocsigenmod.cma: ocsigenmod.cmo ocsigen.cmo ocsigenboxes.cmo
	$(CAMLC) -a -o ocsigenmod.cma ocsigenmod.cmo ocsigen.cmo \
	ocsigenboxes.cmo

ocsigenboxes.cmo:
	$(OCAMLFIND) ocamlc $(DBG) $(LIB) $(PP2) -c ocsigenboxes.ml

ocsigenduce.cma: xhtml1_strict.cmo ocsigenduce.cmo
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -a -o ocsigenduce.cma xhtml1_strict.cmo ocsigenduce.cmo 
xhtml1_strict.cmo:
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c xhtml1_strict.ml

#xhtml1_strict.cmi:
#	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c xhtml1_strict.mli

ocsigenduce.cmo:
	$(OCAMLDUCEFIND) ocamlc $(DBG) $(LIB) -c ocsigenduce.ml

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
	-rm -f *.cm[aiox] *~ doc/* $(NAME) *.annot

.PHONY: install
install:
	@echo "Installing binary"
	mkdir -p $(PREFIX)/$(BINDIR)
	$(INSTALL) -m 755 $(NAME) $(PREFIX)/$(BINDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/$(BINDIR)/$(NAME)

depend:
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) $(EXTENSIONS:.ml=.mli) $(EXTENSIONS) > .depend

FORCE:

include .depend


