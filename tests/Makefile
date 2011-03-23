include ../Makefile.config

## Use local files
## (tests do not require global installation of Eliom)
export OCAMLPATH := ${SRC}/src/files:${OCAMLPATH}

OCAMLC   := $(OCAMLFIND) ocamlc${BYTEDBG}
OCAMLOPT := $(OCAMLFIND) ocamlopt ${OPTDBG}
OCAMLDEP := $(OCAMLFIND) ocamldep

ifeq "${NATDYNLINK}" "YES"
all: byte opt
else
all: byte
endif

#### Main site : eliom_testsuite ####

SERVER_PACKAGE  := eliom.server \
	           xmlp4.syntax

CLIENT_PACKAGE  := eliom.client

SERVER_LIBS := ${addprefix -package ,${SERVER_PACKAGE}}
CLIENT_LIBS := ${addprefix -package ,${CLIENT_PACKAGE}}

SERVER_FILES := eliom_testsuite1.ml \
                eliom_testsuite2.ml \

ELIOM_FILES  := eliom_testsuite3.eliom

CLIENT_FILES :=

SERVER_OBJS := ${SERVER_FILES:.ml=.cmo} ${ELIOM_FILES:.eliom=.cmo} eliom_testsuite.cmo
CLIENT_OBJS := ${CLIENT_FILES:.ml=.cmo} ${addprefix syntax_temp_files/,${ELIOM_FILES:.eliom=.cmo}}

STATICDIR := ../local/var/www/tests

byte:: eliom_testsuite.cma ${STATICDIR}/eliom_testsuite.js
opt:: eliom_testsuite.cmxs ${STATICDIR}/eliom_testsuite.js

#### Server side #######

eliom_testsuite.cma: ${SERVER_OBJS}
	${OCAMLC} -a -o $@ $^
eliom_testsuite.cmxa: ${SERVER_OBJS:.cmo=.cmx}
	${OCAMLOPT} -a -o $@ $^

${addprefix syntax_temp_files/,${ELIOM_FILES:.eliom=.type.mli}}: \
syntax_temp_files/%.type.mli: %.eliom
	$(OCAMLC) -thread ${SERVER_LIBS} -package eliom.syntax.type -syntax camlp4o -i -ppopt -impl -impl $< > $@ \
	  || (rm -f $@; exit 1)

${ELIOM_FILES:.eliom=.cmo}: \
%.cmo: %.eliom syntax_temp_files/%.type.mli
	$(OCAMLC) -thread ${SERVER_LIBS} -package eliom.syntax.server -syntax camlp4o \
	  -ppopt -type -ppopt ${<:%.eliom=syntax_temp_files/%.type.mli} -ppopt -impl \
	  -c -impl $< -o $@

${ELIOM_FILES:.eliom=.cmx}: \
%.cmx: %.eliom syntax_temp_files/%.type.mli
	$(OCAMLOPT) -thread ${SERVER_LIBS} -package eliom.syntax.server -syntax camlp4o \
	  -ppopt -type -ppopt ${<:%.eliom=syntax_temp_files/%.type.mli} -ppopt -impl \
	  -c -impl $< -o $@

##### Client side ####

${addprefix syntax_temp_files/,${ELIOM_FILES:.eliom=.cmo}}: \
syntax_temp_files/%.cmo: %.eliom syntax_temp_files/%.type.mli
	$(OCAMLC) ${CLIENT_LIBS} -package eliom.syntax.client -syntax camlp4o \
	  -ppopt -type -ppopt ${<:%.eliom=syntax_temp_files/%.type.mli} -ppopt -impl \
	  -c -impl $< -o $@

syntax_temp_files/eliom_testsuite: ${CLIENT_OBJS}
	${OCAMLC} -verbose -o $@ ${CLIENT_LIBS} -linkpkg $^

eliom_testsuite.js: ../src/client/eliom_client.js syntax_temp_files/eliom_testsuite
	${JS_OF_OCAML} -o $@ -pretty $^

${STATICDIR}/eliom_testsuite.js: eliom_testsuite.js
	cp -f $< $@

####### Aux site: Ocamlduce #######

ifeq "${OCAMLDUCE}" "YES"
byte:: ocamlduce.byte
opt:: ocamlduce.opt
endif

ocamlduce.byte:
	$(MAKE) -C ocamlduce byte

ocamlduce.opt:
	$(MAKE) -C ocamlduce opt

####### Aux site: miniwiki #######

byte:: miniwiki.byte
opt:: miniwiki.opt

miniwiki.byte:
	$(MAKE) -C miniwiki byte

miniwiki.opt:
	$(MAKE) -C miniwiki opt

############

%.cmi: %.mli
	$(OCAMLC) -thread -syntax camlp4o ${SERVER_LIBS} -c $<
%.cmo: %.ml
	$(OCAMLC) -thread -syntax camlp4o ${SERVER_LIBS} -c $<
%.cmx: %.ml
	$(OCAMLOPT) -thread -syntax camlp4o ${SERVER_LIBS} -c $<
%.cmxs: %.cmxa
	$(OCAMLOPT) -shared -linkall -o $@ $<

## Clean up

clean: clean.local
	${MAKE} -C ocamlduce clean
	${MAKE} -C miniwiki clean
clean.local:
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *.annot
	-rm -f eliom_testsuite.js syntax_temp_files/*

distclean: clean.local
	-rm -f *~ \#* .\#*
	${MAKE} -C ocamlduce distclean
	${MAKE} -C miniwiki distclean

## Dependencies

depend:
	${MAKE} -C ocamlduce depend
	${MAKE} -C miniwiki depend
	$(OCAMLDEP) -syntax camlp4o ${LIBS} *.ml *.mli > .depend
	$(OCAMLDEP) -syntax camlp4o ${LIBS} *.ml *.mli > .depend

eliom_testsuite3.cmi: eliom_testsuite1.cmi

FORCE:
-include .depend








# FILES=$(wildcard *.ml)

# #TUTOWIKI= eliom_testsuite11.wiki eliom_testsuite12.wiki eliom_testsuite13.wiki eliom_testsuite14.wiki

# ifeq "$(OCAMLDUCE)" "YES"
# DUCEMODBYTE=exampleduce.cmo
# DUCEMODOPT=$(CMXSDUCE)
# else
# DUCEMODBYTE=
# DUCEMODOPT=
# endif

# ifeq "$(NATDYNLINK)" "YES"
# CMXS= $(FILES:.ml=.cmxs)
# CMXSDUCE=exampleduce.cmxs
# else
# CMXS=
# endif


# LWTDIR := $(shell ocamlfind query lwt)
# REACTDIR := $(shell ocamlfind query react)

# DERIVINGDIR := ../../deriving/tmp/deriving
# ELIOMDIR := ../../eliom
# ELIOMCLIENTDIR := $(ELIOMDIR)/client
# PAELIOMCLIENTDIR := $(ELIOMDIR)/syntax

# JSOFOCAMLDIR := $(shell ocamlfind query js_of_ocaml)

# LIB = $(LIBDIRS2) -package lwt.unix,netstring,camlp4,lwt.react,calendar,deriving

# CLIENTLIB = -package lwt.react,js_of_ocaml,deriving


# CAMLC = $(OCAMLFIND) $(CAMLCNAME) $(DBG) $(LIB)
# CAMLOPT = $(OCAMLFIND) $(CAMLOPTNAME) $(DBG) $(LIB)
# CAMLDEP = $(OCAMLFIND) ocamldep $(LIB)

# PP = -syntax camlp4o -ppopt "../../xmlp4/xhtmlsyntax.cma" -ppopt "-loc loc"

# OBJS = $(FILES:.ml=.cmo)
# OBJSOPT = $(CMXS)

# byte: eliom_testsuite.cma $(OBJS) $(DUCEMODBYTE) miniwiki.byte $(TUTOWIKI) ../../files/tests/eliom_testsuite3.js

# opt: $(OBJSOPT) eliom_testsuite.cmxs $(DUCEMODOPT) miniwiki.opt $(TUTOWIKI) ../../files/tests/eliom_testsuite3.js



# # eliom_testsuite12.wiki: eliom_testsuite1.ml
# # 	cat eliom_testsuite1.ml | sed '/%<||3>%/,$$ d' | sed '1,/%<||2>%/d' | /bin/sh ./tutomake.sh > eliom_testsuite12.wiki

# # eliom_testsuite13.wiki: eliom_testsuite1.ml
# # 	cat eliom_testsuite1.ml | sed '/%<||4>%/,$$ d' | sed '1,/%<||3>%/d' | /bin/sh ./tutomake.sh > eliom_testsuite13.wiki

# # eliom_testsuite14.wiki: eliom_testsuite3.eliom
# # 	cat eliom_testsuite3.eliom | sed '/%<||2>%/,$$ d' | sed '1,/%<||1>%/d' | /bin/sh ./tutomake.sh > eliom_testsuite14.wiki






# eliom_testsuite.cma: eliom_testsuite1.cmo eliom_testsuite2.cmo eliom_testsuite3.cmo eliom_testsuite.cmo
# 	$(OCAMLFIND) ocamlc -a -o $@ $^

# eliom_testsuite.cmxs: eliom_testsuite1.cmx eliom_testsuite2.cmx eliom_testsuite3.cmx eliom_testsuite.cmx
# 	$(OCAMLFIND) ocamlopt -linkall -shared -o $@ $^

# .SUFFIXES:
# .SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs

# .PHONY: depend


# .ml.cmo:
# 	$(CAMLC) $(PP) -c $<

# .mli.cmi:
# 	$(CAMLC) -c $<

# .ml.cmx:
# #	-rm ../../extensions/ocsipersist.cmx
# 	$(CAMLOPT) $(PP) -c $<
# #	touch ../../extensions/ocsipersist.cmx

# .cmx.cmxs:
# 	$(CAMLOPT) -shared -linkall -o $@ $<


# clean:
# 	$(MAKE) -C miniwiki clean
# 	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *~ $(NAME) *.annot ocamlduce/*.cm[ioax] ocamlduce/*.cmxa ocamlduce/*.cmxs ocamlduce/*~ ocamlduce/*.annot eliom_testsuite3.js eliom_testsuite3_client* eliom_testsuite3.ml ../../files/tests/eliom_testsuite3_client.js eliom_testsuite3 syntax_temp_files/*

# depend:
# 	$(MAKE) -C miniwiki depend
# 	$(CAMLDEP) $(PP) $(LIB) $(FILES:.ml=.mli) $(FILES) | sed s%ocsipersist.cmx%ocsipersist.cmi%g > .depend

# FORCE:

# -include .depend


opt::


# %.wiki: %.ml
	# cat $< | sed '1,/(\*wiki\*/d' | sed '/%<||2>%/,$$ d' | /bin/sh ./tutomake.sh > $@

