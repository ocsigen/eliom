CAMLC    = ocamlfind ocamlc
CAMLLEX  = ocamllex
CAMLYACC = ocamlyacc
CAMLDOC  = ocamlfind ocamldoc
FLAGS    = -g
CAMLCOPT = ocamlfind ocamlopt
CAMLDEP  = ocamlfind ocamldep
NAME 	 =  test_pp
LIBDIR 	= ../lib
LIB= -package netstring -I $(LIBDIR)
PP = -pp "camlp4o ../lib/xmllexer.cmo ../lib/xmlparser.cmo" 
ALL_CMO  = http_frame.cmo http_parser.cmo http_lexer.cmo framepp.cmo http_com.cmo sender_helpers.cmo test_pp.cmo

all: byte
	cp *.cmi *.cmo  ../lib

.PHONY: depend

byte: $(ALL_CMO)
	$(CAMLC) $(LIB) -o $(NAME) unix.cma lwt.cma netstring.cma xhtmlpp.cmo $(ALL_CMO)


doc: 
	$(CAMLDOC) $(PP) $(LIB) -html -d documentation *.ml *.mli

clean:
	rm $(NAME) $(OPTNAME) *.cm[oix] *.o \
	http_parser.ml http_parser.mli http_lexer.ml  documentation/*	

viclean: clean
	rm *~

depend:
	$(CAMLDEP) $(PP) *.ml *.mli > .depend

http_parser.cmo:http_parser.cmi http_parser.ml http_parser.mli

http_lexer.cmo:http_lexer.cmi http_lexer.ml

http_lexer.ml: http_lexer.mll

http_parser.ml: http_parser.mly




# generic rules :
#################

.SUFFIXES: .mll .mly .mli .ml .cmi .cmo .cmx

.mll.mli:
	$(CAMLLEX) $<

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
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
