.PHONY: default all clean distclean

PROGRAMS=main

default: all
all: $(PROGRAMS)

main: Main.hs
	ghc -O2 --make $< -o $@

clean:
	$(RM) *.hi *.o *~

distclean: clean
	$(RM) $(PROGRAMS)
