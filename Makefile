#
# Makefile for CCGMS Term, resurrected from antiquity, September 2016
# (C) Craig Smith, 2016
# Permission is granted to modify and redistribute this software under
# the terms of the BSD 3-clause license, see LICENSE.txt for details.
#
# Building requires cc65 6502 cross-compiler package;
# see http://cc65.github.io/cc65 or 'git clone https://github.com/cc65/cc65.git'
# to build yourself.
# Optionally creating a .d64 image of the resulting C64 prg requires the c1541
# utility from VICE in your path.

OUTPUT_PRG	= ccgmsterm
D64IMAGE	= ccgmsterm.d64
SRC		= ccgmsterm.s

LSTF		= $(SRC:.s=.lst)
OBJF		= $(SRC:.s=.o)

OBJDIR		= obj
OBJ		= $(OBJF:%=$(OBJDIR)/%)
LST		= $(LSTF:%=$(OBJDIR)/%)

# Define HISTORICAL=1 below to re-enable script-kiddie protection
# (and a baud rate bug), so the assembled output is exactly like
# the historical CCGMS Term 5.5
# Or just use the 'make history' rule [sorry :-)]
#
HISTORICAL?=0

ifneq (0,$(HISTORICAL))
	HISTARG = -Dhistorical=$(HISTORICAL)
	HISTENCODE = encode
endif

all:	term image
	echo $(OBJ)

term:	$(OUTPUT_PRG) $(HISTENCODE)

$(OBJ):	$(SRC) $(OBJDIR)
	ca65 $(HISTARG) -t c64 -l $(LST) $(SRC) -o $(OBJ)

$(OUTPUT_PRG):	precheck $(OBJDIR) $(OBJ)
	cl65 $(HISTARG) -t c64 -C c64-asm.cfg $(OBJ) -o $(OUTPUT_PRG)
	@echo $(OUTPUT_PRG) created.

$(OBJDIR):
	@mkdir -p $(OBJDIR)

encode: $(OUTPUT_PRG)
	mv $(OUTPUT_PRG) $(OUTPUT_PRG).pre && \
	perl encode.pl $(OUTPUT_PRG).pre > $(OUTPUT_PRG) && \
	rm -f $(OUTPUT_PRG).pre

image:	$(OUTPUT_PRG)
	@c1541 -h >/dev/null 2>&1 || (echo "c1541 not found in path; skipping creation of .d64 image." ; exit 0)
	@dd if=/dev/zero of=$(D64IMAGE) bs=256 count=683 >/dev/null 2>&1
	c1541 -attach $(D64IMAGE) -format "ccgmsterm 5.5,cg" -write $(OUTPUT_PRG) -list
	
history:	# couldn't resist
	@make HISTORICAL=1 OUTPUT_PRG=ccgmsterm5.5 term image
hclean:
	@make HISTORICAL=1 OUTPUT_PRG=ccgmsterm5.5 clean

clean:
	rm -f $(OBJ) $(LST) $(OUTPUT_PRG) $(D64IMAGE) ./*~
	@test -d $(OBJDIR) && rmdir $(OBJDIR) || exit 0

precheck:
	@cl65 >/dev/null 2>/dev/null || (echo "cl65 6502 assembler is not in path." && exit 1)
	@(cl65 -V 2>&1 | egrep 'V2\.1[56789]|V2\.[23456789]|V[3456789]\.') > /dev/null || (echo Requirement ca65 version 2.15 or later not found && cl65 -V && exit 1)

.PHONY:	precheck encode history image

