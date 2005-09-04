
REPS = db obj

all: $(REPS)

.PHONY: $(REPS) clean


db:
	make -C db depend all install

obj:
	make -C obj depend all install

clean:
	@for i in $(REPS) ; do make -C $$i clean ; done

