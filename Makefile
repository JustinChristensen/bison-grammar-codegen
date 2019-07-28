PROG := bison-gen
CABAL := cabal
CURL := curl
BISON := ./bison/src/bison

.PHONY: all
all: build

.PHONY: build
build: $(PROG)

.PHONY: $(PROG)
$(PROG):
	$(CABAL) v2-build

.PHONY: run
run:
	$(CABAL) v2-run

.PHONY: repl
repl:
	$(CABAL) v2-repl

gram.y: GRAM_FILE:= https://raw.githubusercontent.com/postgres/postgres/REL_11_4/src/backend/parser/gram.y
gram.y:
	$(CURL) -sO $(GRAM_FILE)

.PHONY: test
test: gram.y
	@grammar=$$($(BISON) -fgrammar-only gram.y 2>/dev/null) && \
	printf "%s" "$$grammar" | cabal v2-run || \
	printf "bison not found at $(BISON) \n"

.PHONY: clean
clean:
	rm -rf dist dist-newstyle

