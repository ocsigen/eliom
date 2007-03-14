include ../Makefile.config

all:
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml all
else 
	$(MAKE) -C newocaml all
endif

clean:
	touch oldocaml/.depend
	touch newocaml/.depend
	-$(MAKE) -C oldocaml clean
	-$(MAKE) -C newocaml clean

depend:
	touch oldocaml/.depend
	touch newocaml/.depend
ifeq "$(OCAMLVERSION)" "OLD"
	$(MAKE) -C oldocaml depend
else 
	$(MAKE) -C newocaml depend
endif
