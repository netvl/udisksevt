# udisksevt project makefile
# Copyright (C) DPX-Infinity, 2010

## Setup some variables
# Find all .hs files
FILES=$(shell find -type f -iname '*.hs')
# Binary output directory
OUTDIR=out
# Name of ipupdd binary
BINNAME=$(OUTDIR)/udisksevt
# GHC extensions used in project - as flags
GHCEXTS=

## Rules

# Main rule
all: udisksevt

# Rule for updater project
udisksevt: $(FILES)
	ghc $(GHCEXTS) --make Main.hs -o $(BINNAME)

# Clean rule
clean:
	rm -f $(FILES:.hs=.o)
	rm -f $(FILES:.hs=.hi)
	rm -f *~

# Clean rule for output directory
outclean:
	rm -rf $(OUTDIR)/*
