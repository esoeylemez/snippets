.PHONY: all

targets = \
	$(patsubst %.hs,%,$(wildcard [a-z]*.hs)) \
	$(patsubst %.c,%,$(wildcard [a-z]*.c))


all: $(targets)


CFLAGS = --std=gnu99 -O2
GHCFLAGS = -O -threaded -rtsopts -fdefer-typed-holes


longest-palindrome-c: CFLAGS += -Wno-unused-result
%: %.c Makefile
	gcc $(CFLAGS) -o $@ $<

pe74: GHCFLAGS += -O2 -fllvm
%: %.hs Makefile
	@mkdir -p tmp/$@
	ghc $(GHCFLAGS) -outputdir=tmp/$@ -o $@ $<
	@touch $@
