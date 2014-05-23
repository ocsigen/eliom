#### Distillery ####

.PHONY: byte opt

DISTILLERY := eliom-distillery

basic:
	mkdir -p $@
	$(DISTILLERY) -name distillery_basic -target-directory $@
	echo 'PREFIX=local/' >> $@/Makefile.options
	echo 'STATICDIR=var/www/distillery-basic/static' >> $@/Makefile.options
	echo 'ELIOMSTATICDIR=var/www/distillery-basic/eliomstatic' >> $@/Makefile.options
	echo 'LIBDIR=var/www/distillery-basic/lib' >> $@/Makefile.options
	echo 'WWWUSER=' >> $@/Makefile.options
	echo 'WWWGROUP=' >> $@/Makefile.options
	make -C $@ byte install.static

byte: | basic
	make -C basic byte install.lib.byte
opt: | basic
	make -C basic opt install.lib.opt

clean: | basic
	make -C basic clean
distclean: | basic
	make -C distbasic clean
