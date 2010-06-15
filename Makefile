# udisksevt project makefile
# Copyright (C) DPX-Infinity, 2010

## Setup some variables
# Find all .hs files
FILES=$(shell find -type f -iname '*.hs')
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

# Clean rule
clean:
	rm -f $(FILES:.hs=.o)
	rm -f $(FILES:.hs=.hi)
	rm -rf dist
	rm -f *~

# Clean rule for output directory for direct build
outclean:
	rm -rf $(OUTDIR)
