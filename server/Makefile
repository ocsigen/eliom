include ../Makefile.config

NAME=$(OCSIGENNAME)

FILES= error_pages.ml preemptive.ml pagesearch.ml ocsigen.ml ocsigenboxes.ml parseconfig.ml server.ml

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

INSTALL = install
CAMLC = $(OCAMLFIND) ocamlc $(DBG) $(LIB)
CAMLOPT = $(OCAMLFIND) ocamlopt $(DBG) $(LIB)
CAMLDOC = $(OCAMLFIND) ocamldoc $(LIB)
CAMLDEP = $(OCAMLFIND) ocamldep
ifeq "$(OCSIMORE)" "YES"
OCSIMOREPCKG = 
# dbi.postgresql,
else
OCSIMOREPCKG =
endif
LIB = -thread -package $(OCSIMOREPCKG)netstring -package ssl -I ../lib -I +camlp4
PP = -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc"

CMAO = nums.cma dynlink.cma unix.cma ssl.cma pcre.cma netstring.cma \
       threads.cma \
       ocsimisc.cmo \
       gramlib.cma xmllexer.cmo simplexmlparser.cmo \
       ocsiconfig.cmo messages.cmo \
       lwt.cma preemptive.cmo \
       netlwtstream.cmo multipart.cmo \
       http_frame.cmo http_lexer.cmo http_parser.cmo framepp.cmo http_com.cmo \
       xhtml.cma sender_helpers.cmo\
       error_pages.cmo

OCSIMORECMAO = 
# postgresql.cma dbi.cma dbi_postgresql.cmo
# Last line is only for ocsimore.
# The alternative is to load them in the config file
# (if unsafe_modules are allowed)

OBJS = $(FILES:.ml=.cmo)

all: $(OBJS) $(NAME)
#	cp *.cmo *.cmi ../lib
	cp ocsigen.cmo ocsigen.cmi preemptive.cmi pagesearch.cmi ../lib
#	cp config.cmi ../lib
	cp ocsigenboxes.cmi ../lib
	cp $(NAME) ../bin

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.PHONY: doc depend

$(NAME): $(OBJS)
	@if (test '$(OCSIMORE)' = 'YES');\
	then echo "Ocsigen server with ocsimore support";\
	$(CAMLC) -o $(NAME) -linkall $(CMAO) $(OCSIMORECMAO) $(OBJS);\
	else echo "Ocsigen server without ocsimore support";\
	$(CAMLC) -o $(NAME) -linkall $(CMAO) $(OBJS);\
	fi

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
	$(CAMLDEP) $(PP) $(FILES:.ml=.mli) $(FILES) > .depend

FORCE:

include .depend


