include ../Makefile.config

ifeq "$(DEBUG)" "YES"
DBG = -dtypes -g
else
DBG =
endif

CAMLC    = ocamlfind ocamlc $(DBG)
CAMLLEX  = ocamllex
CAMLYACC = ocamlyacc
CAMLDOC  = ocamlfind ocamldoc
FLAGS    = -g
CAMLCOPT = ocamlfind ocamlopt $(DBG)
CAMLDEP  = ocamlfind ocamldep
NAME 	 = test_pp
LIBDIR 	= ../lib
LIB= -package netstring -I $(LIBDIR) -I +camlp4
PP = -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc" 
ALL_CMO  = ocsiconfig.cmo messages.cmo http_frame.cmo http_parser.cmo http_lexer.cmo framepp.cmo http_com.cmo sender_helpers.cmo #test_pp.cmo

all: byte
	cp *.cmi *.cmo  ../lib

.PHONY: depend

byte: $(ALL_CMO)
#	$(CAMLC) $(LIB) -o $(NAME) unix.cma lwt.cma netstring.cma xhtml.cma $(ALL_CMO)


doc: 
	$(CAMLDOC) $(PP) $(LIB) -html -d documentation *.ml *.mli

clean:
	-rm $(NAME) $(OPTNAME) *.cm[oix] *.o *.annot \
	http_parser.ml http_parser.mli http_lexer.ml ocsiconfig.ml documentation/*	

viclean: clean
	-rm *~

depend:
	$(CAMLDEP) $(PP) *.ml *.mli > .depend

http_parser.cmo:http_parser.ml http_parser.mli http_parser.cmi

http_lexer.cmo:http_lexer.ml http_lexer.cmi messages.cmi

http_lexer.ml: http_lexer.mll

http_parser.ml: http_parser.mly

ocsiconfig.ml: ocsiconfig.ml.IN
	cat ocsiconfig.ml.IN \
	| sed s°0000000000000000°`cat ../VERSION`° \
	| sed s°_WARNING_°"Warning: this file has been generated from ocsiconfig.ml.IN - DO NOT MODIFY MANUALLY!"° \
	| sed s°_LOGDIR_°$(LOGDIR)° \
	| sed s°_CONFIGDIR_°$(CONFIGDIR)° \
	| sed s°_STATICPAGESDIR_°$(STATICPAGESDIR)° \
	> ocsiconfig.ml


# generic rules :
#################

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx

.mll.mli:
	$(CAMLLEX) $<

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

.mly.mli:
	$(CAMLYACC) $<

.mli.cmi:
	$(CAMLC) -c $(FLAGS) $(LIB) $<

.ml.cmi:
	$(CAMLC) -c $(FLAGS) $(LIB) $<

.ml.cmo:
	-rm $(LIBDIR)/$(<:.ml=.cmo)
	-rm $(LIBDIR)/$(<:.ml=.cmi)
	$(CAMLC) $(PP) -c $(FLAGS) $(LIB) $<

.ml.o:
	$(CAMLCOPT) -c $(FLAGS) $(LIB) $<

.ml.cmx:
	$(CAMLCOPT) $(PP) -c $(PROFILE) $(FLAGS) $(LIB) $<

include .depend
