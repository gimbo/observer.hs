PACKAGE		:=	simple-observer
SRCDIR		:=	Control

## Everything from here on should be generic across projects.

SOURCES		:=	$(shell find $(SRCDIR) -name "*.hs" -or -name "*.lhs")
HADDOCK		:=	dist/doc/html/$(PACKAGE)/index.html

all:			clean configure build haddock sdist

clean:
			@runghc Setup clean && \
			  find . -name '*~' -exec rm -vf {} ';' && \
			  find . -name '*.hi' -exec rm -vf {} ';' && \
			  find . -name '*.o' -exec rm -vf {} ';' && \
			  rm -rvf graphmod* SourceGraph*

## For some reason I have trained myself to type "make nuke".
nuke:			clean

## Configuration.

dist/setup-config:	$(PACKAGE).cabal
			runghc Setup configure

configure:		$(SOURCES) dist/setup-config

## Binaries.

dist/build:		$(SOURCES) dist/setup-config
			runghc Setup build

build:			dist/build

## Documentation.

$(HADDOCK):		$(SOURCES) dist/setup-config
			runghc Setup haddock --hyperlink-source

haddock:		$(HADDOCK)

view:			$(HADDOCK)
			open $(HADDOCK)

## Package.

sdist:			dist/setup-config
			runghc Setup sdist

## Hlint, woo.

lint:
			hlint $(SOURCES)

## Plot module dependencies.

graphmod:
			@graphmod -q --no-cluster $(SOURCES) > graphmod.dot && \
			  dot -T pdf -o graphmod.pdf graphmod.dot && \
			  open graphmod.pdf

sourcegraph:
			@SourceGraph $(PACKAGE).cabal

.PHONY: all clean nuke configure build haddock view sdist lint graphmod
