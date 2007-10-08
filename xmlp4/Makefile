include ../Makefile.config

byte:
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml byte
else 
	$(MAKE) -C newocaml byte
endif

opt:
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml opt
else 
	$(MAKE) -C newocaml opt
endif

clean:
#	touch oldocaml/.depend
#	touch newocaml/.depend
	-rm -f *.cm[ioax] *.cmxa *.cmxs *.o *.a *~ *.annot
	-$(MAKE) -C oldocaml clean
	-$(MAKE) -C newocaml clean

depend:
#	touch oldocaml/.depend
#	touch newocaml/.depend
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml depend
else 
	$(MAKE) -C newocaml depend
endif
