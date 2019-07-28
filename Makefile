PROG := bison-gen

.PHONY: all
all: $(PROG)

.PHONY: $(PROG)
$(PROG):
	cabal v2-build

.PHONY: run
run:
	cabal v2-run

.PHONY: clean
clean:
	rm -rf dist dist-newstyle

