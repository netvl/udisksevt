# udisksevt project makefile
# Copyright (C) DPX-Infinity, 2010

## Setup some variables
# Find all .hs files
FILES=$(shell find -type f -iname '*.hs')
# Binary output directory
OUTDIR=out
# Name of ipupdd binary
BINNAME=$(OUTDIR)/udisksevt
# Additional GHC flags
GHCFLAGS=-XTupleSection -XOverloadedStrings

## Rules

# Main rule
all: udisksevt

# Rule for updater project
udisksevt-direct: $(FILES)
	ghc $(GHCFLAGS) --make Main.hs -o $(BINNAME)

configure:
	runhaskell Setup.hs configure

udisksevt: $(FILES)
	runhaskell Setup.hs build

# Clean rule
clean:
	rm -f $(FILES:.hs=.o)
	rm -f $(FILES:.hs=.hi)
	rm -rf dist
	rm -f *~

# Clean rule for output directory
outclean:
	rm -rf $(OUTDIR)/*
