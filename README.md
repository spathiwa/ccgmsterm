
**CCGMS Term _(6502 assembly source for Commodore 64)_**

**Copyright (C) Craig Smith, 1986-1988, 2016**

*Resurrected and released to world September 5, 2016.
Permission is granted to modify and distribute this work under
the terms of the MIT license.
See LICENSE file for details.*

Version 5.5 (ported to build with ca65)

This was the last authentic/official version released by me in January 1988.
Over the years others created various hacked versions, both good and bad.
CCGMSTerm 6.01, for example, was a hacked version of (I think) 4.0, pre-
autodialer, and all they did was change my name to "Chris" in the banner
and add a sda decompression wrapper so the disk image was smaller.
CCGMSTerm 7 fixed timing issues with 2400 baud and CCGMS+RLE-8.0 added RLE 
image support.  I do not have corresponding source for any of these
changes, but if people want to submit useful changes, your contributions
are welcome!

The file ccgmsterm.s contains combined source from the original SEQ files
(named 5a.gs,5b.gs,5c.gs and 5d.gs) in CBM Assembler 64 format and chained
together using .FIL directives because the entire source could not
fit into a C64's memory using the CBM editor.  The encoding of this
file has been converted to ASCII (with lowercase labels), and strings
containing the PETSCII left arrow character ($5F) have been changed
to its numeric code so modern or cross-platform assemblers do not
treat it as an underscore.  (The original source files are in the
original folder for posterity.)

This source can be compiled using the ca65 portable 6502 compiler.
Except for the conversion to ASCII and the addition of feature
and conditional directives (.feature/.if/.endif), the format of
this file remains compatible with CBM Assembler 64.
(The .feature directives below enable this compatibility.)

*DEPENDENCIES*

You need ca65 (https://github.com/cc65/cc65.git) to build this,
and the c1541 tool from the VICE emulator (http://vice-emu.sourceforge.net)
to create a .d64 disk image out of it.

*BUILDING*

Just type 'make' to build ccgmsterm, minus the "script kiddie" trivial
encoding scheme that used to protect it from being easily modified with a disk
editor.  You can type "make history" to build a version that still
has this (as well as a baud rate bug on PAL systems that's also fixed
in the normal build; thanks to Jim Drew for discovering this).



