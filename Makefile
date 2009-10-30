SOURCES		:=	$(shell find Control -name "*.hs")
HASKELLS	:=	$(shell find . -name "*.hs")

clean:
			@runghc Setup clean && \
			  find . -name '*~' -exec rm -vf {} ';' && \
			  find . -name '*.hi' -exec rm -vf {} ';' && \
			  find . -name '*.o' -exec rm -vf {} ';' && \
			  rm -vf graphmod*

nuke:			clean

lint:
			hlint $(HASKELLS)

configure:
			runghc Setup configure

build:
			runghc Setup build

haddock:
			runghc Setup haddock --hyperlink-source

view:
			open dist/doc/html/fsmActions/index.html

sdist:
			runghc Setup sdist

.PHONY: testsuite/Main
testsuite/Main:
			ghc -O2 -Wall -itestsuite --make testsuite/Main

test:			testsuite/Main
			./testsuite/Main

testslow:
			sh testsuite/runtests.sh

all:			test clean configure build haddock sdist

# Plotting module dependencies

graphmod:
			@graphmod -q --no-cluster $(SOURCES) > graphmod.dot && \
			  dot -T pdf -o graphmod.pdf graphmod.dot && \
			  open graphmod.pdf
