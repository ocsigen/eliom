include ../Makefile.config

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

CAMLC    = $(OCAMLFIND) $(CAMLCNAME2) $(DBG)
CAMLDOC  = $(OCAMLFIND) ocamldoc
FLAGS    =
CAMLCOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG)
CAMLDEP  = $(OCAMLFIND) ocamldep $(LIBDIRS)
NAME 	 = test_pp
LIB= -package netstring,ssl,lwt $(LIBDIRS) -I +camlp4
PP =
# -pp "camlp4o ../lib/xhtmlsyntax.cma -- -loc loc" 
ALL_CMO = ocsigen_stream.cmo multipart.cmo http_headers.cmo		\
           ocsigen_http_frame.cmo http_parser.cmo http_lexer.cmo	\
           framepp.cmo ocsigen_http_com.cmo ocsigen_charset_mime.cmo	\
           ocsigen_senders.cmo ocsigen_headers.cmo #test_pp.cmo

ALL_CMX  = $(ALL_CMO:.cmo=.cmx)

.PHONY: depend

byte: ocsigen_stream.cmi $(ALL_CMO)
#	$(CAMLC) $(LIB) -o $(NAME) unix.cma lwt.cma netstring.cma xhtml.cma $(ALL_CMO)
#	cp *.cmi *.cmo  ../lib

opt: ocsigen_stream.cmi $(ALL_CMX)
#	cp *.cmi *.cmx *.o ../lib


doc: 
	$(CAMLDOC) $(PP) $(LIB) -html -d documentation *.ml *.mli

clean:
	-rm $(NAME) $(OPTNAME) *.cm[oiax] *.cmxa *.o *.annot \
	http_parser.ml http_parser.mli http_lexer.ml documentation/*	

viclean: clean
	-rm *~

depend:
	$(CAMLDEP) $(PP) $(LIBDIRS) *.ml *.mli *.mll *.mly > .depend

http_parser.cmo:http_parser.ml http_parser.mli http_parser.cmi
http_parser.cmx:http_parser.ml http_parser.mli http_parser.cmi

http_lexer.cmo:http_lexer.ml http_lexer.cmi
http_lexer.cmx:http_lexer.ml http_lexer.cmi

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

.mly.mli:
	$(CAMLYACC) $<

.mli.cmi:
	$(CAMLC) -c $(FLAGS) $(LIB) $<

.ml.cmi:
	$(CAMLC) -c $(FLAGS) $(LIB) $<

.ml.cmo:
#	-rm $(LIBDIR)/$(<:.ml=.cmo)
#	-rm $(LIBDIR)/$(<:.ml=.cmi)
	$(CAMLC) $(PP) -c $(FLAGS) $(LIB) $<

.ml.o:
	$(CAMLCOPT) -c $(FLAGS) $(LIB) $<

.ml.cmx:
	$(CAMLCOPT) $(PP) -c $(PROFILE) $(FLAGS) $(LIB) $<

-include .depend
