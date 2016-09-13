.PHONY: all

targets = \
	$(patsubst %.hs,%,$(wildcard [a-z]*.hs)) \
	$(patsubst %.c,%,$(wildcard [a-z]*.c))


all: $(targets)


CFLAGS = --std=gnu99 -O2
GHCFLAGS = -O -threaded -rtsopts -fdefer-typed-holes


%: %.c Makefile
	gcc $(CFLAGS) -o $@ $<

%: %.hs Makefile
	@mkdir -p tmp/$@
	ghc $(GHCFLAGS) -outputdir=tmp/$@ -o $@ $<
	@touch $@
