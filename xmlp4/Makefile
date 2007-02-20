include ../Makefile.config

all:
	@if (test '$(OCAMLVERSION)' = 'OLD');\
	then $(MAKE) -C oldocaml all;\
	else $(MAKE) -C newocaml all;\
	fi

clean:
	touch oldocaml/.depend
	touch newocaml/.depend
	-$(MAKE) -C oldocaml clean
	-$(MAKE) -C newocaml clean

depend:
	touch oldocaml/.depend
	touch newocaml/.depend
	@if (test '$(OCAMLVERSION)' = 'OLD');\
	then $(MAKE) -C oldocaml depend;\
	else $(MAKE) -C newocaml depend;\
	fi
