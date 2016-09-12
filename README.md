
**CCGMS Term _(6502 assembly source for Commodore 64)_**
**Copyright (C) Craig Smith, 1986-1988, 2016**

*Resurrected and released to world September 5, 2016.
Permission is granted to modify and distribute this work under
the terms of the GNU General Public License, version 3.
See LICENSE file for details.*

Version 5.5 (ported to build with ca65)

The file ccgmsterm.s contains combined source from the original SEQ files
(named 5a.gs,5b.gs,5c.gs and 5d.gs) in CBM Assembler 64 format and chained
together using .FIL directives because the entire source could not
fit into a C64's memory using the CBM editor.  The encoding of this
file has been converted to ASCII (with lowercase labels), and strings
containing the PETSCII left arrow character ($5F) have been changed
to its numeric code so modern or cross-platform assemblers do not
treat it as an underscore.

This source can be compiled using the ca65 portable 6502 compiler.
Except for the conversion to ASCII and the addition of feature
and conditional directives (.feature/.if/.endif), the format of
this file remains compatible with CBM Assembler 64.
(The .feature directives below enable this compatibility.)

*DEPENDENCIES*

You need ca65 (https://github.com/cc65/cc65.git) to build this,
and the c1541 tool from the VICE emulator (http://vice-emu.sourceforge.net)
to create a .d64 disk image out of it.
