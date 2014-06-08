TARGETS=miniwiki testsuite distillery

.PHONY: clean distclean byte opt
clean:
	for i in $(TARGETS); do $(MAKE) -C $$i clean; done
distclean: clean
	for i in $(TARGETS); do $(MAKE) -C $$i distclean; done

byte:
	for i in $(TARGETS); do $(MAKE) -C $$i byte; done
opt: clean
	for i in $(TARGETS); do $(MAKE) -C $$i opt; done
