include ../Makefile.config

byte:
	make -C xhtml byte
	make -C syntax byte
	make -C xml-pretty byte

opt: 
	make -C xhtml opt
	make -C syntax opt
	make -C xml-pretty opt

clean: 
	rm *cm* *.o -f *.a
	make -C xhtml clean
	make -C syntax clean
	make -C xml-pretty clean

depend: 
	make -C xhtml depend
	make -C syntax depend
	make -C xml-pretty depend

xmlp4pre.byte:
	make -C xhtml byte
	make -C syntax byte
	make -C syntax predep
xmlp4pre.opt: 
	make -C xhtml byte
	make -C syntax byte
	make -C syntax predep