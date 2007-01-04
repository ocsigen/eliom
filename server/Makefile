include ../Makefile.config

NAME=$(OCSIGENNAME)

FILES= error_pages.ml preemptive.ml pagegen.ml parseconfig.ml server.ml \
       ocsigenmod.ml ocsigen.ml staticmod.ml ocsigenboxes.ml

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

INSTALL = install
CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
LIB = -thread -package netstring,netsys,ssl -I ../lib -I +camlp4
PP = 
PP2 = -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc"

CMAO = nums.cma dynlink.cma unix.cma \
       ssl.cma pcre.cma netsys.cma netstring.cma \
       threads.cma \
       ocsimisc.cmo \
       gramlib.cma xmllexer.cmo simplexmlparser.cmo \
       ocsiconfig.cmo messages.cmo \
       lwt.cma preemptive.cmo \
       pagegen.cmo parseconfig.cmo \
       ocsistream.cmo multipart.cmo \
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtml.cma predefined_senders.cmo\
       error_pages.cmo

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS) $(NAME)
#	cp *.cmo *.cmi ../lib
	cp ocsigen.cmo ocsigen.cmi ocsigenmod.cmo ocsigenmod.cmi staticmod.cmo staticmod.cmi preemptive.cmi pagegen.cmi ../lib
#	cp config.cmi ../lib
	cp ocsigenboxes.cmi ../lib
	cp $(NAME) ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	$(CAMLC) -o $(NAME) -linkall $(CMAO) $(OBJS);\

ocsigenboxes.cmo:
	$(OCAMLFIND) ocamlc $(DBG) $(LIB) $(PP2) -c ocsigenboxes.ml

.ml.cmo:
	$(CAMLC) $(PP) -c $<

.mli.cmi:
	$(CAMLC) -c $<
.ml.cmx:
	$(CAMLOPT) $(PP) -c $<

doc:
	$(CAMLDOC) -d doc -html ocsigen.mli

.PHONY: clean
clean:
	-rm -f *.cm[iox] *~ doc/* $(NAME) *.annot

.PHONY: install
install:
	@echo "Installing binary"
	mkdir -p $(BINDIR)
	$(INSTALL) -m 755 $(NAME) $(BINDIR)/

.PHONY: uninstall
uninstall:
	rm -f $(BINDIR)/$(NAME)

depend:
	$(CAMLDEP) $(PP2) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


