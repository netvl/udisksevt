# udisksevt project makefile
# Copyright (C) DPX-Infinity, 2010

## Setup some variables
# Find all .hs files
FILES=$(shell find -type f -iname '*.hs')
# Temporary files
TEMPFILES=$(shell find -type f -iname '*~' -o -iname '*.swp')
# Extract udisksevt version
VERSION=$(shell grep -E '^Version' udisksevt.cabal | awk '{print $$2}')
# Distribution directory name
DISTDIR=udisksevt-$(VERSION)
# Binary output directory
OUTDIR=out
# Name of udisksevt binary for direct build
BINNAME=$(OUTDIR)/udisksevt
# Additional GHC flags
GHCFLAGS=-XTupleSection -XOverloadedStrings

## Rules

# Main rule
all: udisksevt

# Rule for direct build, i.e. without cabal
udisksevt-direct: $(FILES)
	ghc $(GHCFLAGS) --make Main.hs -o $(BINNAME)

# Configure cabal project
configure:
	runhaskell Setup.hs configure

# Build cabal project
udisksevt: $(FILES)
	runhaskell Setup.hs build

# Create current version tarball
distfile:
	rm -rf /tmp/$(DISTDIR)
	mkdir /tmp/$(DISTDIR)
	cp -r LICENSE Main.hs Makefile README Setup.hs udisksevt.cabal udisksevt.conf UDisksEvt /tmp/$(DISTDIR)
	tar cvjf $(DISTDIR).tar.bz2 -C /tmp $(DISTDIR)

# Clean rule
clean:
	rm -f $(FILES:.hs=.o)
	rm -f $(FILES:.hs=.hi)
	rm -rf dist
	rm -f $(TEMPFILES)

# Clean rule for output directory for direct build
outclean:
	rm -rf $(OUTDIR)

# Test variables
test:
	@echo Files:
	@echo $(FILES)
	@echo Temporary files:
	@echo $(TEMPFILES)
	@echo Version:
	@echo $(VERSION)
	@echo Distribution directory:
	@echo $(DISTDIR)
