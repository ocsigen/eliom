VERSION = $(shell bash -c '\
        version=`ocamlc -version`; \
        n1=$${version:0:1}; \
        n2=$${version:2:2}; \
        n3=$${version:5}; \
        if [ $$n1 = "3" ] && [ $$n2 -le 9 ]; then \
                echo "OLD"; \
        else \
                echo "NEW"; \
        fi' )

all:
	@if (test '$(VERSION)' = 'OLD');\
	then make -C oldocaml all;\
	else make -C newocaml all;\
	fi

clean:
	rm .depend
	touch oldocaml/.depend
	touch newocaml/.depend
	make -C oldocaml clean
	make -C newocaml clean
	rm oldocaml/.depend
	rm newocaml/.depend

depend:
	> oldocaml/.depend
	> newocaml/.depend
	@if (test '$(VERSION)' = 'OLD');\
	then make -C oldocaml depend;\
	else make -C newocaml depend;\
	fi
