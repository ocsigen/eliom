#### Distillery ####

.PHONY: byte opt clean

DISTILLERY := eliom-distillery
ALL = basic mobile

byte:
	for i in $(ALL); do $(MAKE) byte-$$i; done

opt:
	for i in $(ALL); do $(MAKE) opt-$$i; done

distillery-%:
	mkdir -p $@
	$(DISTILLERY) -name distillery_$* -template $* -target-directory $@
	echo 'TEST_PREFIX=local/' >> $@/Makefile.options
	echo 'PREFIX=../../../local/' >> $@/Makefile.options
	echo 'STATICDIR=var/www/$@/static' >> $@/Makefile.options
	echo 'ELIOMSTATICDIR=var/www/$@/eliomstatic' >> $@/Makefile.options
	echo 'LIBDIR=var/www/$@/lib' >> $@/Makefile.options
	echo 'WWWUSER=' >> $@/Makefile.options
	echo 'WWWGROUP=' >> $@/Makefile.options
	make -C $@ byte install.static

byte-%: | distillery-%
	make -C distillery-$* byte install.byte
opt-%: | distillery-%
	make -C distillery-$* opt install.opt

clean:
	for i in $(ALL); do rm -Rf distillery-$$i clean; done

distclean: clean
