;
; CCGMS Term (6502 assembly source for Commodore 64)
; Copyright (C) Craig Smith, 1986-1988, 2016
; All rights reserved.
;
; Resurrected and released to world September 5, 2016.
; Permission is granted to modify and distribute this work under
; the terms of the BSD 3-clause license.
; See the LICENSE.txt file for details.
;
; Version 5.5/5.5+ (ported to build with ca65)
;
; This file contains combined source from the original SEQ files (named
; 5a.gs,5b.gs,5c.gs and 5d.gs) in CBM Assembler 64 format and chained
; together using .FIL directives because the entire source could not
; fit into a C64's memory using the CBM editor.  The encoding of this
; file has been converted to ASCII (with lowercase labels), and strings
; containing the PETSCII left arrow character ($5F) have been changed
; to its numeric code so modern or cross-platform assemblers do not
; treat it as '_'.
;
; This source can be compiled using the ca65 portable 6502 compiler.
; Except for the conversion to ASCII and the addition of feature
; and conditional directives (.feature/.if/.endif), the format of
; this file remains compatible with CBM Assembler 64.
; (The .feature directives below enable this compatibility.)
;

.feature labels_without_colons 
.feature loose_string_term

; Conditional compilation symbols:
v55plus  = 1	; Enable the bug fix and minor changes made in 5.5+
toward24 = 1	; Enable "Toward 2400" new modem chrout/chkin/nmi routines by George Hug
hack24   = 0	; Enable 2400 baud hack from alwyz

.if .not(.defined(historical)) .or .blank(historical)
historical = 0	; define to 1 to make this source produce an exact image of ccgms term 5.5, bug per bug.
;setting it to zero fixes the bdpal bug, removes internal padding between
;the punter code and ccgms code, and disabled the 'script kiddie' protection
;(see 'decode' routine)
.endif
.if historical
.feature pc_assignment
.endif

;.opt err        ;no list!!!
modreg = $dd01
datdir = $dd03
rdtim  = $ffde
frmevl = $ad9e
outnum = $bdcd
nmivec = $0318
status = $90
lognum = $05
modem  = $02
secadr = $03
setlfs = $ffba
setnam = $ffbd
open   = $ffc0
chkin  = $ffc6
chkout = $ffc9
chrin  = $ffcf
chrout = $ffd2
getin  = $ffe4
close  = $ffc3
clrchn = $ffcc
clall  = $ffe7
readst = $ffb7
plot   = $fff0
listen = $ffb1
second = $ff93
talk   = $ffb4
tksa   = $ff96
unlsn  = $ffae
untlk  = $ffab
acptr  = $ffa5
ciout  = $ffa8
numfil = $98
locat  = $fb
nlocat = $fd
xmobuf = $fd
inpbuf = $0200
backgr = $d021
border = $d020
textcl = 646
clcode = $e8da
scrtop = 648
line   = 214
column = 211
llen   = 213
qmode  = 212
imode  = 216
bcolor = 0
tcolor = 15
cursor = 95    ;cursor "_"
left   = $9d
cursfl = $fe
buffst = $b2
bufptr = $b0
grasfl = $0313
duplex = $12
tempch = $05
tempcl = $06
mulfil = $c200
rinput = $cf00
routpt = $ce00
bufend = routpt
mulcnt = 2047
mulfln = 2046
mlsall = 2045
mulskp = 2044
max    = $02
begpos = $07
endpos = $ac
bufflg = $0b
buffl2 = $0c
buffoc = $10
pnt10  = $0200
pnt11  = $028d
pnt12  = $029b
pnt13  = $029c
pnt14  = $02a1
.if historical
pbuf   = $1000 ; else defined later, right before 'start'
;there was no good reason for this arbitrary location, and it made us set
;the pc to leave a gap for it.
.endif
pbuf2  = $0400 ; when this buffer was moved to screen memory, the gap where
;it was used to be wasn't closed, leaving the area at label 'garbage'
xmoscn = pbuf2
can    = 24
ack    = 6
nak    = 21
eot    = 4
soh    = 1
cpmeof = 26
;
;Commodore Color Graphics
;Manipulation System Terminal
;by Craig Smith
;
;version 2016 -- sep 2016
;version 5.5+ -- feb 1988
;version 5.5 -- jan 1988
;version 5.0 -- jan 1988
;version 4.5 -- may 1987
;version 4.1 -- oct 1986
;version 4.0 -- date unknown
; mods by greg pfoutz,w/permission
;version 3.0 -- aug 1985
;
.if historical
*=$0801
.else
.org $0801
.endif
.byt $0d,$08,$0a,00,$9e,$32,$30
.byt $36,$33,20,$39,00,00,00
 jmp start
;
punter ; source code $0812
;referenced by old $c000 addresses
;*=$0812  ;pxxxxx
p49152  lda #$00
 .byt $2c
p49155  lda #$03
 .byt $2c
p49158  lda #$06
 .byt $2c
p49161  lda #$09
 .byt $2c
p49164  lda #$0c
 .byt $2c
p49167  lda #$0f
 nop
p49170  jmp pnt23
p49173  jmp pnt109
pnt23 sta $62
 tsx
 stx pbuf+28
 lda #<pnttab
 clc
 adc $62
 sta pntjmp+1
 lda #>pnttab
 adc #$00
 sta pntjmp+2
pntjmp jmp pnttab
pnttab jmp pnt28
 jmp pnt87
 jmp pnt84
 jmp pnt95
 jmp pnt99
 jmp pnt110
pnt27 .byt 'goobadacks/bsyn'
pnt28 sta pbuf+5
 lda #$00
 sta pbuf
 sta pbuf+1
 sta pbuf+2
pnt29 lda #$00
 sta pbuf+6
 sta pbuf+7
pnt30 jsr pnt114
 jsr pnt38
 lda $96
 bne pnt35
 lda pbuf+1
 sta pbuf
 lda pbuf+2
 sta pbuf+1
 lda pnt10
 sta pbuf+2
 lda #$00
 sta pbuf+4
 lda #$01
 sta pbuf+3
pnt31 lda pbuf+5
 bit pbuf+3
 beq pnt33
 ldy pbuf+4
 ldx #$00
pnt32 lda pbuf,x
 cmp pnt27,y
 bne pnt33
 iny
 inx
 cpx #$03
 bne pnt32
 jmp pnt34
pnt33 asl pbuf+3
 lda pbuf+4
 clc
 adc #$03
 sta pbuf+4
 cmp #$0f
 bne pnt31
 jmp pnt111
pnt34 lda #$ff
 sta pbuf+6
 sta pbuf+7
 jmp pnt30
pnt35 inc pbuf+6
 bne pnt36
 inc pbuf+7
pnt36 lda pbuf+7
 ora pbuf+6
 beq pnt37
 lda pbuf+6
 cmp #$07
 lda pbuf+7
 cmp #$14
 bcc pnt30
 lda #$01
 sta $96
 jmp pnt101
pnt37 lda #$00
 sta $96
 rts
 nop
pnt38 jmp pnt122
 nop
 nop
pnt39 cmp pnt13
 beq pnt40
 ldy pnt13
 lda ($f7),y
 pha
 inc pnt13
 lda #$00
 sta $96
 pla
 sta pnt10
 pla
 tay
 jmp pnt41
pnt40 lda #$02
 sta $96
 lda #$00
 sta pnt10
 pla
 tay
pnt41 pha
 lda #$03
 sta $ba
 pla
 rts
pnt42 ldx #$05
 jsr chkout
 ldx #$00
pnt43 lda pnt27,y
 jsr chrout
 iny
 inx
 cpx #$03
 bne pnt43
 jmp pnt118
pnt44 sta pbuf+8
 lda #$00
 sta pbuf+11
pnt45 lda #$02
 sta $62
 ldy pbuf+8
 jsr pnt42
pnt46 lda #$04
 jsr pnt28
 lda $96
 beq pnt47
 dec $62
 bne pnt46
 jmp pnt45
pnt47 ldy #$09
 jsr pnt42
 lda pbuf+13
 beq pnt48
 lda pbuf+8
 beq pnt50
pnt48 lda pbuf2+4
 sta pbuf+9
 sta pbuf+23
 jsr pnt65
 lda $96
 cmp #$01
 beq pnt49
 cmp #$02
 beq pnt47
 cmp #$04
 beq pnt49
 cmp #$08
 beq pnt47
pnt49 rts
pnt50 lda #$10
 jsr pnt28
 lda $96
 bne pnt47
 lda #$0a
 sta pbuf+9
pnt51 ldy #$0c
 jsr pnt42
 lda #$08
 jsr pnt28
 lda $96
 beq pnt52
 dec pbuf+9
 bne pnt51
pnt52 rts
pnt53 lda #$01
 sta pbuf+11
pnt54 lda pbuf+30
 beq pnt55
 ldy #$00
 jsr pnt42
pnt55 lda #$0b
 jsr pnt28
 lda $96
 bne pnt54
 lda #$00
 sta pbuf+30
 lda pbuf+4
 cmp #$00
 bne pnt59
 lda pbuf+13
 bne pnt61
 inc pbuf+25
 bne pnt56
 inc pbuf+26
pnt56 jsr pnt79
 ldy #$05
 iny
 lda ($64),y
 cmp #$ff
 bne pnt57
 lda #$01
 sta pbuf+13
 lda pbuf+22
 eor #$01
 sta pbuf+22
 jsr pnt79
 jsr pnt77
 jmp pnt58
pnt57 jsr pnt74
pnt58 lda #'-'
 .byt $2c
pnt59 lda #':'
 jsr pnt107
 ldy #$06
 jsr pnt42
 lda #$08
 jsr pnt28
 lda $96
 bne pnt58
 jsr pnt79
 ldy #$04
 lda ($64),y
 sta pbuf+9
 jsr pnt80
 ldx #$05
 jsr chkout
 ldy #$00
pnt60 lda ($64),y
 jsr pnt123
 iny
 cpy pbuf+9
 bne pnt60
 jsr clrchn
 lda #$00
 rts
pnt61 lda #'*'
 jsr pnt107
 ldy #$06
 jsr pnt42
 lda #$08
 jsr pnt28
 lda $96
 bne pnt61
 lda #$0a
 sta pbuf+9
pnt62 ldy #$0c
 jsr pnt42
 lda #$10
 jsr pnt28
 lda $96
 beq pnt63
 dec pbuf+9
 bne pnt62
pnt63 lda #$03
 sta pbuf+9
pnt64 ldy #$09
 jsr pnt42
 lda #$00
 jsr pnt28
 dec pbuf+9
 bne pnt64
 lda #$01
 rts
pnt65 ldy #$00
pnt66 lda #$00
 sta pbuf+6
 sta pbuf+7
pnt67 jsr pnt114
 jsr pnt38
 lda $96
 bne pnt70
 lda pnt10
 sta pbuf2,y
 cpy #$03
 bcs pnt68
 sta pbuf,y
 cpy #$02
 bne pnt68
 lda pbuf
 cmp #$41
 bne pnt68
 lda pbuf+1
 cmp #$43
 bne pnt68
 lda pbuf+2
 cmp #$4b
 beq pnt69
pnt68 iny
 cpy pbuf+9
 bne pnt66
 lda #$01
 sta $96
 rts
pnt69 lda #$ff
 sta pbuf+6
 sta pbuf+7
 jmp pnt67
pnt70 inc pbuf+6
 bne pnt71
 inc pbuf+7
pnt71 lda pbuf+6
 ora pbuf+7
 beq pnt73
 lda pbuf+6
 cmp #$06
 lda pbuf+7
 cmp #$10
 bne pnt67
 lda #$02
 sta $96
 cpy #$00
 beq pnt72
 lda #$04
 sta $96
pnt72 jmp pnt101
pnt73 lda #$08
 sta $96
 rts
pnt74 lda pbuf+22
 eor #$01
 sta pbuf+22
 jsr pnt79
 ldy #$05
 lda pbuf+25
 clc
 adc #$01
 sta ($64),y
 iny
 lda pbuf+26
 adc #$00
 sta ($64),y
 ldx #$02
 jsr chkin
 ldy #$07
pnt75 jsr chrin
 sta ($64),y
 iny
 jsr readst
 bne pnt76
 cpy pbuf+24
 bne pnt75
 tya
 pha
 jmp pnt78
pnt76 tya
 pha
 ldy #$05
 iny
 lda #$ff
 sta ($64),y
 jmp pnt78
pnt77 pha
pnt78 jsr clrchn
 jsr pnt109
 jsr pnt103
 jsr pnt109
 ldy #$04
 lda ($64),y
 sta pbuf+9
 jsr pnt80
 pla
 ldy #$04
 sta ($64),y
 jsr pnt81
 rts
pnt79 lda #<pbuf2
 sta $64
 lda pbuf+22
 clc
 adc #>pbuf2
 sta $65
 rts
pnt80 lda #<pbuf2
 sta $64
 lda pbuf+22
 eor #$01
 clc
 adc #>pbuf2
 sta $65
 rts
pnt81 lda #$00
 sta pbuf+18
 sta pbuf+19
 sta pbuf+20
 sta pbuf+21
 ldy #$04
pnt82 lda pbuf+18
 clc
 adc ($64),y
 sta pbuf+18
 bcc pnt83
 inc pbuf+19
pnt83 lda pbuf+20
 eor ($64),y
 sta pbuf+20
 lda pbuf+21
 rol a
 rol pbuf+20
 rol pbuf+21
 iny
 cpy pbuf+9
 bne pnt82
 ldy #$00
 lda pbuf+18
 sta ($64),y
 iny
 lda pbuf+19
 sta ($64),y
 iny
 lda pbuf+20
 sta ($64),y
 iny
 lda pbuf+21
 sta ($64),y
 rts
pnt84 lda #$00
 sta pbuf+13
 sta pbuf+12
 sta pbuf+29
 lda #$01
 sta pbuf+22
 lda #$ff
 sta pbuf+25
 sta pbuf+26
 jsr pnt80
 ldy #$04
 lda #$07
 sta ($64),y
 jsr pnt79
 ldy #$05
 lda #$00
 sta ($64),y
 iny
 sta ($64),y
pnt85 jsr pnt53
 beq pnt85
pnt86 lda #$00
 sta pnt10
 rts
pnt87 lda #$01
 sta pbuf+25
 lda #$00
 sta pbuf+26
 sta pbuf+13
 sta pbuf+22
 sta pbuf2+5
 sta pbuf2+6
 sta pbuf+12
 lda #$07
 sta pbuf2+4
 lda #$00
pnt88 jsr pnt44
 lda pbuf+13
 bne pnt86
 jsr pnt93
 bne pnt92
 jsr clrchn
 lda pbuf+9
 cmp #$07
 beq pnt90
 ldx #$02
 jsr chkout
 ldy #$07
pnt89 lda pbuf2,y
 jsr chrout
 iny
 cpy pbuf+9
 bne pnt89
 jsr clrchn
pnt90 lda pbuf2+6
 cmp #$ff
 bne pnt91
 lda #$01
 sta pbuf+13
 lda #'*'
 .byt $2c
pnt91 lda #'-'
 jsr goobad
 jsr pnt109
 lda #$00
 jmp pnt88
pnt92 jsr clrchn
 lda #':'
 jsr goobad
 lda pbuf+23
 sta pbuf2+4
 lda #$03
 jmp pnt88
pnt93 lda pbuf2
 sta pbuf+14
 lda pbuf2+1
 sta pbuf+15
 lda pbuf2+2
 sta pbuf+16
 lda pbuf2+3
 sta pbuf+17
 jsr pnt79
 lda pbuf+23
 sta pbuf+9
 jsr pnt81
 lda pbuf2
 cmp pbuf+14
 bne pnt94
 lda pbuf2+1
 cmp pbuf+15
 bne pnt94
 lda pbuf2+2
 cmp pbuf+16
 bne pnt94
 lda pbuf2+3
 cmp pbuf+17
 bne pnt94
 lda #$00
 rts
pnt94 lda #$01
 rts
pnt95 lda #$00
 sta pbuf+25
 sta pbuf+26
 sta pbuf+13
 sta pbuf+22
 sta pbuf+12
 lda #$07
 clc
 adc #$01
 sta pbuf2+4
 lda #$00
pnt96 jsr pnt44
 lda pbuf+13
 bne pnt98
 jsr pnt93
 bne pnt97
 lda pbuf2+7
 sta pbuf+27
 lda #$01
 sta pbuf+13
 lda #$00
 jmp pnt96
pnt97 lda pbuf+23
 sta pbuf2+4
 lda #$03
 jmp pnt96
pnt98 lda #$00
 sta pnt10
 rts
pnt99 lda #$00
 sta pbuf+13
 sta pbuf+12
 lda #$01
 sta pbuf+22
 sta pbuf+29
 lda #$ff
 sta pbuf+25
 sta pbuf+26
 jsr pnt80
 ldy #$04
 lda #$07
 clc
 adc #$01
 sta ($64),y
 jsr pnt79
 ldy #$05
 lda #$ff
 sta ($64),y
 iny
 sta ($64),y
 ldy #$07
 lda pbuf+27
 sta ($64),y
 lda #$01
 sta pbuf+30
pnt100 jsr pnt53
 beq pnt100
 lda #$00
 sta pnt10
 rts
pnt101 inc pbuf+12
 lda pbuf+12
 cmp #$03
 bcc pnt102
 lda #$00
 sta pbuf+12
 lda pbuf+11
 beq pnt103
 bne pnt106
pnt102 lda pbuf+11
 beq pnt106
pnt103 ldx #$00
pnt104 ldy #$00
pnt105 iny
 bne pnt105
 inx
 cpx #$78
 bne pnt104
pnt106 rts
pnt107 pha
 lda pbuf+25
 ora pbuf+26
 beq pnt108
 lda pbuf+29
 bne pnt108
 pla
 jsr goobad
 pha
pnt108 pla
 rts
pnt109 jsr $ef7e
 lda pnt14
 cmp #$80
 beq pnt109
 cmp #$92
 beq pnt109
 rts
pnt110 rts
pnt111 ldx #$00
pnt112 lda pbuf2,x
 cmp #$0d
 bne pnt113
 inx
 cpx #$03
 bcc pnt112
 jmp pnt120
pnt113 jmp pnt29
pnt114 lda pnt11
 cmp #$02
 bne pnt116
pnt115 pla
 tsx
 cpx pbuf+28
 bne pnt115
pnt116 lda #$01
 sta pnt10
pnt117 rts
 brk
pnt118 jsr clrchn
pnt119 lda $dd01
 and #$00   ;and #$10 for carrier
 beq pnt117 ;check and abort
pnt120 tsx
 cpx pbuf+28
 beq pnt121
 pla
 sec
 bcs pnt120
pnt121 lda #$80
 sta pnt10
 jsr clrchn
 rts
pnt122 tya
 pha
 jsr pnt119
 lda pnt12
 jmp pnt39
pnt123 pha
 jsr pnt119
 pla
 jmp chrout
 brk
ptrtxt .byt 13,13,5,'NEW Punter ',00
upltxt .byt 'Up',00
dowtxt .byt 'Down',00
lodtxt .byt 'load.',13,00
flntxt .byt 'Enter filename: ',00
xfrmed .byt 13,158,32,32,0
xfrtxt .byt 'loading: ',159,0
xf2txt .byt 13,5,'  (','Press C= to abort.)',13,13,00
abrtxt .byt 'Aborted.',13,00
mrgtxt .byt 153,32,'Good Blocks: ',5,'000',5,'   -   '
 .byt 153,'Bad Blocks: ',5,'000',13,0
gfxtxt .byt 153,'Graphics',00
asctxt .byt 159,'ASCII',00
rdytxt .byt ' Terminal Ready.',155,13,13,00
dsctxt .byt 13,5,'Disconnected.',13,13,0
drtype .byt 'd','s','p','u','r'
drtyp2 .byt 'e','e','r','s','e'
drtyp3 .byt 'l','q','g','r','l'
drform .byt 158,2,157,157,5,6,32,159,14,153,32,63,32,0
;the following was garbage memory in the gap between the punter
;source at *=$0812 and the main ccgms source (originally *=$1020).
;the last 32 bytes, from $1000-$101f, were pbuf, a buffer used by
;punter. the rest of it used to be pbuf2, a larger punter buffer,
;but it was moved to screen memory so download progress would be
;more entertaining and the gap at its former location never closed..
;apparently the cbm assembler left uninitialized memory when
;you reset the pc by using *=addr rather than filling with zeroes.
;the following bytes are what happened to be there in the ccgms 
;term 5.5 that was distributed, and i've reproduced those bytes here
;so this source will assemble to produce that image exactly.
.if historical
garbage .byt $bd,$80 
 .byt $08,$c9,$20,$f0,$04,$e8,$4c,$f6,$0e,$8e,$6e,$08,$a9,$00,$8d,$5a
 .byt $08,$b0,$03,$4c,$a1,$15,$a2,$19,$a0,$02,$b9,$08,$09,$d1,$08,$d0
 .byt $16,$88,$10,$f6,$8a,$0a,$aa,$bd,$11,$20,$85,$08,$bd,$12,$20,$85
 .byt $09,$ad,$74,$08,$6c,$08,$00,$a5,$08,$18,$69,$03,$85,$08,$90,$02
 .byt $e6,$09,$ca,$10,$d3,$4c,$a1,$15,$a9,$01,$2c,$a9,$03,$2c,$a9,$02
 .byt $8d,$65,$08,$a0,$00,$8c,$5a,$08,$a8,$c0,$03,$d0,$01,$88,$8c,$7a
 .byt $08,$20,$d1,$15,$b0,$08,$ae,$6a,$08,$a9,$07,$4c,$c8,$15,$8c,$7a
 .byt $08,$ae,$69,$08,$20,$bc,$16,$ce,$57,$08,$30,$35,$f0,$03,$4c,$2b
 .byt $10,$a9,$01,$8d,$71,$08,$c9,$01,$d0,$09,$ac,$65,$08,$8c,$7a,$08
 .byt $4c,$95,$0f,$ac,$7a,$08,$ad,$71,$08,$ae,$69,$08,$20,$68,$1b,$a0
 .byt $00,$8c,$5a,$08,$ac,$7a,$08,$c0,$03,$d0,$59,$ce,$7a,$08,$4c,$03
 .byt $10,$a9,$04,$8d,$71,$08,$ae,$65,$08,$e0,$03,$d0,$0c,$ad,$6c,$08
 .byt $ac,$5a,$08,$20,$90,$28,$ee,$5a,$08,$ad,$6d,$08,$ac,$5a,$08,$20
 .byt $90,$28,$ee,$5a,$08,$ae,$65,$08,$e0,$02,$d0,$0c,$ad,$6c,$08,$ac
 .byt $5a,$08,$20,$90,$28,$ee,$5a,$08,$ad,$75,$08,$29,$09,$d0,$97,$e0
 .byt $01,$d0,$05,$ad,$6c,$08,$d0,$8e,$a9,$00,$aa,$ac,$7a,$08,$8d,$5a
 .byt $08,$20,$68,$1b,$20,$35,$16,$b0,$03,$4c,$cb,$15,$bd,$80,$08,$e8
 .byt $8e,$6e,$08,$ec,$77,$08,$f0,$05,$90,$03,$4c,$9c,$15,$c9,$2c,$d0
 .byt $e3
;end of garbage
*=$1020  ; skip original punter buffers
.else ; !historical
pbuf .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.endif ; historical

;start of terminal program
start
;undocumented opcodes to obscure program transfer to the 'decode'
;routine and then to 'stadec'.
.if historical
.byt $af,$f1,$e4,$ab,$ff,$18,$8b,>dummyb,$44,$4c,$48,$a9,<dummyb,$7a
.byt $48,$a9,<decode,$8f,$ff,$02,$3c,$78,$40,$a9,>decode,$8f,$00
.byt $02,$e2,$41,$6c,$ff,$02
dummyb .byt 0
.endif
stadec
 sei
.if toward24 .and(.not(historical))
 jsr rssetup
.endif
.if historical
 lda #<disnmi
 sta nmivec
 lda #>disnmi
 sta nmivec+1
.endif
 cld
 ldx #$ff
 txs
 lda #$2f
 sta $00
 lda #$37
 sta $01
 lda #1
 sta 204
 lda #bcolor  ;settup
 sta backgr
 sta border
 lda #tcolor
 sta textcl
 lda #$80
 sta 650      ;rpt
 lda #$0e
 sta $d418
 lda #$00
 sta locat
 lda #$e0       ;clear secondary
 sta locat+1    ;screens
 lda #$20
 ldy #$00
erasl1
 sta (locat),y
 iny
 bne erasl1
 inc locat+1
 bne erasl1
 cli
 lda $ba        ;current dev#
 cmp #$08
 bcc stodev
 cmp #$0c
 bcc stodv2
stodev lda #$08
stodv2 sta diskdv
 cmp #$08
 beq stodv3
 jsr drvchk
 bmi stodev
stodv3
 lda #<endprg   ;set buffer start
 sta buffst
 lda #>endprg
 sta buffst+1
 lda newbuf     ;init. buffer
 sta bufptr     ;& open rs232
 lda newbuf+1
 sta bufptr+1
 jsr rsopen
 jmp init
rsopen          ;open rs232 file
 jsr clall
 lda #lognum
 ldx #modem
 ldy #secadr
 jsr setlfs
 lda protoe
 ldx #<proto
 ldy #>proto
 jsr setnam
 jsr open
 lda #>rinput  ;move rs232 buffers
 sta 248       ;so they don't
 lda #>routpt  ;overlap termbuffer
 sta 250
ercopn
 lda #$02       ;open err chan
 ldx #<dreset
 ldy #>dreset
 jsr setnam
 lda #15
 ldx diskdv
 tay
 jsr setlfs
 jmp open
init
 jsr selmdm
 lda #1
 sta cursfl     ;non-destructive
 lda #0
 sta $9d ;prg mode
 sta grasfl     ;grafix mode
 sta allcap     ;upper/lower
 sta buffoc     ;buff closed
 sta duplex     ;full duplex
 lda alrlod
 bne noload
 jsr $e544  ;clr
 lda #1
 sta alrlod
 ldx #<conffn
 ldy #>conffn
 lda #11
 jsr setnam
 lda #2
 ldx diskdv
 ldy #0
 jsr setlfs
 jsr loadcf
 jmp begin
noload
 jsr baud
begin
 jsr bell
term
 jsr mssg       ;title screen
 jsr instr
 jmp main
proto  .byt $08   ;start with
proto1 .byt $00   ;1200 baud setng
bdoutl .byt $51
bdouth .byt $0d
protoe .byt $02 ;length of proto
dreset .byt 'u9'
diskdv .byt $08
allcap .byt 0
alrlod .byt 0
lastch .byt 0
newbuf .byt <endprg,>endprg
main
 ldx #$ff
 txs
 lda #$48
 sta 655
 lda #$eb
 sta 656
 jsr clrchn
 jsr curprt
main2
 lda bufptr
 sta newbuf
 lda bufptr+1
 sta newbuf+1
 jsr clrchn
 jsr getin
 cmp #$00
 bne specck
mainab
 jmp main3
;check special-keys here
specck
 cmp #6
 bne specc1
 ldx 653
 cpx #6
 bne specc1
 ldx #16
 stx datdir
 ldx #0
 stx modreg
 jmp main2
specc1
 cmp #$5f
 bne chkscr
 ldx 653     ;shift <- toggles
 beq checkf  ;n/d cursor
 cpx #1
 beq spetog
 lda allcap
 eor #$01
 sta allcap
 jmp main2
spetog
 jmp crsrtg
;
chkscr
 ldx 653
 cpx #$05    ;shift-ctrl and 1-4
 bcc chekrs  ;toggle screen
 ldx #$03
chksc1
 cmp clcode,x ;table of color codes
 beq chksc2
 dex
 bpl chksc1
 jmp main3   ;(not in range)
chksc2
 jmp scrtog  ;x holds pos 0-3
;
chekrs
 cmp #131    ;shift-r/s
 bne checkf  ;to hang-up
 jmp hangup
checkf       ;f-keys
 cmp #133
 bcc notfky
 cmp #141
 bcs notfky
 ldx #0
 stx $d020
 stx $d021
 pha
 jsr curoff
 pla
 sec
 sbc #133
 sta $03
 asl $03
 clc
 adc $03
 sta fbranc+1
 clc
fbranc
 bcc fbranc+2
 jmp f1
 jmp f3
 jmp f5
 jmp f7
 jmp f2
 jmp f4
 jmp f6
 jmp f8
notfky
 ldx allcap
 beq upplow
 ldx 53272
 cpx #23
 bne upplow
 cmp #'a'
 bcc upplow
 cmp #'['  ;'z'+1
 bcs upplow
 ora #$80
upplow
 sta $03
 ldx grasfl
 beq mainop
 jsr catosa  ;convert to ascii
 bne mainop
mnback
 jmp main2
mainop
 pha
 ldx #lognum
 jsr chkout
 pla
 jsr chrout
 ldx grasfl
 beq maing
 jsr satoca
 sta $03
 bne maing
 jmp main2
maing
 ldx duplex
 beq main3
 jsr clrchn  ;if half duplex
 lda $03     ;bring back char
 ldx grasfl
 beq mainb
 cmp #$5f
 bne mainb
 lda #164    ;echo underline for
 sta $03     ;<- in ascii/half dup
mainb
 jmp bufchk  ;skip modem input
main3
 jsr clrchn
 ldx 653
 cpx #4   ;ctrl pressed
 bne specc2
 ldx 197  ;fn key
 cpx #3
 bcc specc2
 cpx #7
 bcs specc2
 lda #0
 sta macmdm
 jsr prtmac
 jmp main
specc2
 cpx #3 ;shift,c=
 bne specc3
 ldx 657
 bpl specc3
 ldx #23
 stx 53272
specc3
 ldx #lognum
 jsr chkin
 jsr getin
 cmp #$00
 beq mnback
 ldx status
 bne mnback
 pha
 jsr clrchn
 pla
nopass
 ldx grasfl
 beq main4
 jsr satoca   ;ascii to c=
 beq main3
main4
 cmp #20      ;delete from modem
 bne bufchk   ;becomes false del
 lda #$5f
bufchk
 jsr putbuf
 jmp contn
putbuf
 ldx buffoc
 beq buffot
 ldx bufptr
 cpx #<bufend
 bne bufok
 ldx bufptr+1
 cpx #>bufend
 beq buffot
bufok
 ldy #$00
 sta (bufptr),y
 inc bufptr
 bne buffot
 inc bufptr+1
buffot rts
contn
 jsr ctrlck
 bcc contn2
 jmp main
ctrlck
 cmp #$0a   ;ctrl-j
 beq swcrsr
 cmp #$0b   ;ctrl-k
 bne nonchk
swcrsr
 ldx grasfl
 bne nonchk
 pha
 jsr curoff
 pla
 and #$01   ;form to ch flag
 eor #$01
 sta cursfl
swcext
 sec
 rts
nonchk
 cmp #14  ;ctrl-n
 bne ctrlen
 ldx #0
 stx $d020
 stx $d021
ctrlen
 cmp #$07   ;ctrl-g
 bne ctrleo
 jsr bell
ctrleo
 cmp #22   ;ctrl-v
 bne ctrlev
 jsr gong
ctrlev
 cmp #$15   ;ctrl-u
 bne ctrle2
 ldx #21
 stx 53272
 bne ctrlex
ctrle2
 cmp #$0c   ;ctrl-l
 bne ctrle3
 ldx #23
 stx 53272
 bne ctrlex
ctrle3
 cmp #$5f   ;false del
 bne ctrle4 ;(buff and 1/2 duplx)
 lda #20
 bne ctrlex
ctrle4
 ldx lastch
 cpx #2  ;ctrl-b
 bne ctrlex
 ldx #15
ctrlb1  cmp clcode,x
 beq ctrlb2
 dex
 bpl ctrlb1
 bmi ctrlex
ctrlb2 stx $d020
 stx $d021
 lda #16  ;ctrl-p..non printable
ctrlex
 sta lastch
 clc
 rts
contn2
 pha
 jsr curoff  ;get rid of cursor
 pla
 jsr chrout
 jsr qimoff
 jmp main
;
;end of term
;subroutines follow:
;
bell
 ldx #$09
 stx 54291
 ldx #00
 stx 54292
 ldx #$40
 stx 54287
 ldx #00
 stx 54290
 ldx #$11
 stx 54290
 rts
gongm1 .byt 24,6,13,20,4,11,18,15,8,1,5,19,12,14,7,0,4,11,18,24
gongm2 .byt 47,0,0,0,0,0,0,4,8,16,13,13,11,28,48,68,21,21,21,15
gong
 pha
 ldx #0
gong1
 lda gongm1,x
 tay
 lda gongm2,x
 sta 54272,y
 inx
 cpx #20
 bcc gong1
 pla
 rts
scrtog   ;toggle screen #1-4
 txa     ;(swap screen memory with
 pha     ; behind kernal rom)
 jsr curoff
 lda 653
 sta $04
 pla
 asl a
 asl a
 asl a
 clc
 adc #$e0
 sta locat+1
 lda #$04
 sta $03
 lda #$00
 sta locat
 sta $02
 sei
 lda $d011
 pha
 lda #$0b
 sta $d011
 lda #<ramnmi
 sta $fffa
 lda #>ramnmi
 sta $fffb
 lda #$2f
 sta $00
 lda #$35
 sta $01
scrtg1
 jsr scrnl1
 cmp #$08
 bcc scrtg1
 lda #$d8
 sta $03
scrtg2
 jsr scrnl1
 cmp #$dc
 bcc scrtg2
 pla
 sta $d011
 lda #$37
 sta $01
 cli
 jmp main
ramnmi
 sta tempch
 lda #$37
 sta $01
 plp
 php
 sta tempcl
 lda #>ramnm2
 pha
 lda #<ramnm2
 pha
 lda tempcl
 pha
 lda tempch
 jmp $fe43
ramnm2
 pha
 lda #$35
 sta $01
 pla
 rti
scrnl1
 ldx $04
 cpx #05
 beq scrnls
 ldy #0
scrnlc  lda ($02),y
 sta (locat),y
 dey
 bne scrnlc
 beq scrnl3
scrnls  ldy #$00
scrnl2  ;swap screen page
 lda ($02),y
 tax
 lda (locat),y
 sta ($02),y
 txa
 sta (locat),y
 iny
 bne scrnl2
scrnl3  lda #<ramnmi
 sta $fffa
 lda #>ramnmi
 sta $fffb
 inc locat+1
 inc $03
 lda $03
 rts
outspc
 lda #29   ;crsr right
outsp1
 jsr chrout
 dex
 bne outsp1
 rts
bufclr
 lda buffst
 sta bufptr
 lda buffst+1
 sta bufptr+1
 rts
finpos ;calculate screenpos
 ldy line
 lda $ecf0,y
 sta locat
 lda $d9,y
 and #$7f
 sta locat+1
 lda column
 cmp #40
 bcc finp2
 sbc #40
 clc
finp2
 adc locat
 sta locat
 lda locat+1
 adc #$00
 sta locat+1
 ldy #$00
 lda (locat),y
 rts
fincol   ;calculate color ptr
 jsr finpos
 lda #$d4
 clc
 adc locat+1
 sta locat+1
 lda (locat),y
 rts
qimoff   ;turn quote/insert off
 lda #$00
 sta qmode
 sta imode
 rts
mssg
 lda #<msgtxt
 ldy #>msgtxt
 jsr outstr
 lda #32
 jsr chrout
 ldx #18
 lda #163
mslop1
 jsr chrout
 dex
 bne mslop1
 lda #<author
 ldy #>author
 jsr outstr
 ldx #40
mslop2
 lda #183
 jsr chrout
 dex
 bne mslop2
 rts
instr
 lda #<instxt
 ldy #>instxt
 jsr outstr
 lda #<instx2
 ldy #>instx2
 jsr outstr
trmtyp
 ldx grasfl
 bne asctrm
 lda #<gfxtxt
 ldy #>gfxtxt
 bne termtp
asctrm
 lda #<asctxt
 ldy #>asctxt
termtp
 jsr outstr
 lda #<rdytxt
 ldy #>rdytxt
 jmp outstr
msgtxt
.byt 13,$93,8,5,14,18,32,28,' C ',129,' C ',158,' G ',30
.byt ' M ',31,' S ',156,' ! ',5,32
.if .not(historical)
.byt 'Terminal Vers 2016a ',00
.else
.if v55plus
.byt ' Terminal Vers 5.5+ ',00
.else
.byt ' Terminal Vers 5.5  ',00
.endif
.endif
author  .byt '    by Craig Smith   ',146,151,00
;
instxt
.byt 5,'  ',18,'F1',146,32,150,'Upload          '
.byt 5,18,'F2',146,32,150,'Send/Read file',13
.byt 5,'  ',18,'F3',146,32,158,'Download        '
.byt 5,18,'F4',146,32,158,'Buffer commands',13
.byt 5,'  ',18,'F5',146,32,153,'Disk Command    '
.byt 5,18,'F6',146,32,153,'Directory',13
.byt 5,'  ',18,'F7',146,32,30,'Dialer/Params   '
.byt 5,18,'F8',146,32,30,'Switch terms',13,0
instx2
.byt 31,'C',28,'=',5,18,'F1',146,32,159,'Multi-Send    '
.byt 31,'C',28,'=',5,18,'F3',146,32,159,'Multi-Receive',13
.byt 31,'C',28,'=',5,18,'F5',146,32,154,'Send Dir.     '
.byt 31,'C',28,'=',5,18,'F7',146,32,154,'Screen to Buff.',13,13,0
;
mlswrn .byt 13,5,'Buffer too big - Save or Clear first!',13,0
;
dirmdm .byt 0
;directory routine
dirfn   .byt '$'
dir
 lda #$0d
 ldx diskdv
 ldy #$00
 jsr setlfs
 jsr drvchk
 bpl dirst
 jmp drexit
dirst
 jsr clrchn
 jsr cosave
 lda #$0d
 jsr chrout
 jsr open
 lda #0
 sta dirmdm
 lda 653
 cmp #2   ;c= f6
 bne dirlp0
 lda #1
 sta dirmdm
dirlp0
 ldx #$0d
 jsr chkin
 ldy #03
drlp1
 jsr getch
 dey
 bpl drlp1
 jsr getch
 sta $0b
 jsr getch
 ldx $0b
 jsr outnum
 lda #' '
 jsr chrout
drlp2
 jsr getch
 ldx dirmdm
 beq drlpm
 cmp #0
 beq drlpm2
 cmp #' '
 bcc drlp2
drlpm
 jsr chrout
 bne drlp2
drlpm2
 jsr drret
 ldy #01
 bne drlp1
getch
 jsr getin
 ldx status
 bne drlp3
 cmp #00
 rts
drlp3
 pla
 pla
drexit
 jsr clrchn
 jsr coback
 lda #$0d
 jsr chrout
 jmp close
drret
 lda #$0d
 jsr chrout
 jsr clrchn
 jsr getin
 beq drcont
 cmp #$03
 beq drlp3
 lda #$00
 sta $c6
drwait
 jsr getin
 beq drwait
drcont
 ldx dirmdm
 beq dircoe
 lda #145
 jsr chrout
 lda #3  ;screen
 sta 153 ;def input dev
 ldx #5
 jsr chkout
 ldy #0
drcon2 jsr getin
 jsr chrout
 lda #$fd
 sta $a2
drcodl lda $a2
 bne drcodl
 iny
 cpy #27
 bcc drcon2
 lda #$0d
 jsr chrout
 jsr clrchn
 lda #$0d
 jsr chrout
 ldx #5
 jsr chkin
drcon3  jsr getin
 lda $029b
 cmp $029c
 bne drcon3
dircoe
 jsr clrchn
 ldx #$0d
 jmp chkin
drvchk
 lda #00
 sta status
 lda diskdv
 jsr $ed0c
 lda #$f0
 jsr $edbb
 ldx status
 bmi drc2
 jsr $f654
 lda #$00
drc2  rts
;convert standard ascii to c= ascii
satoca
 cmp #$5f
 bne clab0
 lda #164  ;underline
 bne cexit
clab0
 and #127
 cmp #124
 bcs cexit
 cmp #96
 bcc clab1
 sbc #32
 bne cexit
clab1
 cmp #65
 bcc clab2
 cmp #91
 bcs cexit
 adc #128
 bne cexit
clab2
 cmp #08
 bne clab3
 lda #20
clab3
 cmp #12
 bne clab4
 lda #$93
clab4
 cmp #32     ;don't allow home,
 bcs cexit   ;cd, or cr
 cmp #07
 beq cexit
 cmp #$0d
 beq cexit
 cmp #20
 beq cexit
 bne cerrc
cexit cmp #$00
 rts
cerrc
 lda #$00
 beq cexit
;convert c= ascii to standard ascii
catosa
 cmp #20
 bne alab0
 lda #08    ;delete
 bne aexit
alab0 cmp #164 ;underline
 bne alab1
 lda #$5f
alab1  cmp #65
 bcc cexit  ;if<then no conv
 cmp #91
 bcs alab2
 adc #32    ;lower a...z..._
 bne aexit
alab2  cmp #160
 bne alab3
 lda #32    ;shift to space
 bne aexit
alab3  and #127
 cmp #65
 bcc cerrc
 cmp #96    ;upper a...z
 bcs cerrc
aexit   cmp #$00
 rts
savech
 jsr finpos
 sta tempch
 eor #$80
 sta (locat),y
 jsr fincol
 sta tempcl
 lda textcl
 sta (locat),y
 rts
restch  ;restore char und non-crsr
 jsr finpos
 lda tempch
 sta (locat),y
 jsr fincol
 lda tempcl
 sta (locat),y
 rts
spleft  ;output space, crsrleft
 lda #' '
 jsr chrout
 lda #left
 jmp chrout
curoff
 ldx cursfl
 bne restch
 jsr qimoff
 jmp spleft
curprt
 lda cursfl
 bne nondst
 lda #cursor
 jsr chrout
 lda #left
 jmp chrout
nondst
 jmp savech
input
 jsr inpset
 jmp inputl
inpset
 stx max
 cmp #$00
 beq inpcon
 jsr outstr
inpcon
 jsr clrchn
 sec
 jsr plot
 stx $9e
 sty $9f
 jsr finpos    ;set up begin &
 lda locat+1   ;end of input
 sta begpos+1  ;ptrs
 sta endpos+1
 lda locat
 sta begpos
 clc
 adc max
 sta endpos
 lda endpos+1
 adc #$00
 sta endpos+1
 rts
inputl
 lda #0
 sta 204
 jsr savech
inpwat
 jsr getin
 beq inpwat
 sta $03
 and #127
 cmp #17
 beq inpcud
 cmp #34
 beq inpwat
 cmp #13
 bne inpwt1
 jmp inpret
inpwt1
 lda $03
 cmp #20
 beq inpdel
 cmp #157
 beq inpdel
 and #$7f
 cmp #19
 beq inpcls
 bne inpprc
inpcud
 jsr restch
 lda $03
 cmp #145
 beq inphom
 jsr inpcu1
 jmp inpmov
inpcu1 ldy max
inpcu2
 dey
 bmi inpcu3
 lda (begpos),y
 cmp #' '
 beq inpcu2
inpcu3
 iny
 tya
 clc
 adc $9f
 tay
 rts
inpcls
 jsr restch
 lda $03
 cmp #$93
 bne inphom
 ldy max
 lda #' '
inpcl2 sta (begpos),y
 dey
 bpl inpcl2
inphom
 ldy $9f
inpmov
 ldx $9e
 clc
 jsr plot
 jmp inputl
inpdel
 jsr finpos
 lda locat
 cmp begpos
 bne inprst
 lda locat+1
 cmp begpos+1
 beq inpwat
 bne inprst
inpprc
 jsr finpos
 lda locat
 cmp endpos
 bne inpins
 lda locat+1
 cmp endpos+1
 bne inpins
 jmp inpwat
inpins
 lda $03
 cmp #148
 bne inprst
 dec endpos+1
 ldy #$ff
 lda (endpos),y
 inc endpos+1
 cmp #' '
 beq inprst
 jmp inpwat
inprst
 ldx #$03
 stx 651
 jsr restch
 lda $03
 jsr chrout
 jsr qimoff
 jmp inputl
inpret
 jsr restch
 jsr inpcu1
 cmp 211
 bcc inpre2
 ldx $9e
 clc
 jsr plot
inpre2
 jsr finpos
 lda locat
 sec
 sbc begpos
 pha
 tay
 lda #' '
inpspc
 sta (begpos),y
 cpy max
 beq inpinp
 iny
 bne inpspc
inpinp
 pla
 sta max
 ldx $9e
 ldy $9f
 clc
 jsr plot
 lda #1
 sta 204
 lda #$03
 ldy #$00
 tax
 jsr setlfs
 lda #$00
 jsr setnam
 jsr open
 ldx #$03
 jsr chkin
 ldy #$00
inpsto
 cpy max
 beq inpend
 jsr chrin
 sta inpbuf,y
 iny
 bne inpsto
inpend
 lda #$00
 sta inpbuf,y
 jsr clrchn
 lda #$03
 jsr close
 ldx max
 rts
cosave
 ldx textcl
 stx $04
cochng
 ldx #tcolor
 stx textcl
 rts
coback
 ldx $04
 stx textcl
 rts
f6      ;directory
 lda #$01
 ldx #<dirfn
 ldy #>dirfn
dodir
 jsr setnam
 jsr dir
 jmp main
f8      ;term toggle
 ldx 653
 cpx #2
 bne termtg
 jmp cf7
termtg
 lda grasfl
 eor #$01
 sta grasfl
 jsr bell
 jmp term
crsrtg     ;ascii crsr toggle
 jsr curoff
 lda cursfl
 eor #$01
 sta cursfl
 jmp main
hangup     ;hang up phone
 ldx #$20
 stx datdir
 ldx 653
 cpx #2
 beq pickup
 lda mopo2
 sta modreg
 jsr curoff
 lda #<dsctxt
 ldy #>dsctxt
 jsr outstr
 jmp main
pickup
 lda mopo1
 sta modreg
 jsr curoff
 lda #<pcktxt
 ldy #>pcktxt
 jsr outstr
 jmp main
pcktxt .byt 13,5,'Off-hook.',13,13,0
dsktxt .byt 5,13,'Disk#'
dsktx2 .byt '**>      ',157,157,157,157,157,157,00
dskdtx .byt '8 9 1011'
f5      ;disk command
 jsr cosave
dskcmd
 lda diskdv
 and #$07
 asl a
 tay
 lda dskdtx,y
 sta dsktx2
 lda dskdtx+1,y
 sta dsktx2+1
 lda #<dsktxt
 ldy #>dsktxt
 ldx #31
 jsr input
 beq drverr
 lda inpbuf
 cmp #'#'
 beq chgdev
 jsr drvchk
 bmi drvext
 lda #$0d
 jsr chrout
 lda inpbuf
 cmp #'$'
 bne drvsnd
 lda max
 ldx #<inpbuf
 ldy #>inpbuf
 jmp dodir
drvsnd
 ldx diskdv
 stx 612  ;dev# table, log#15
 ldx #$0f
 jsr chkout
 ldx #$00
drvlop
 lda inpbuf,x
 jsr chrout
 inx
 cpx max
 bne drvlop
 lda #$0d
 jsr chrout
drvext
 jsr clrchn
 jsr coback
 lda #$0d
 jsr chrout
 jmp main
drverr
 jsr drvchk
 bmi drvext
 jsr clrchn
 ldx #$0f
 jsr chkin
drver2
 jsr getin
drver3
 jsr chrout
 cmp #$0d
 bne drver2
 beq drvext
chgdev
 ldy #$01
 ldx inpbuf,y
 txa
 and #$0f
 cpx #'1'
 bne chgdv2
 clc
 adc #$09
chgdv2   iny
 clc
 adc inpbuf,y
 and #$0f
 cmp #$08
 bcc drvext
 cmp #$0c
 bcs drvext
 tay
 lda diskdv
 pha
 sty diskdv
 sty 612
 jsr drvchk
 bmi chgdv3
 pla
 lda #145
 jsr chrout
 jmp dskcmd
chgdv3
 pla
 sta diskdv
 sta 612
chgdv4
 lda #' '
 jsr chrout
 lda #'-'
 jsr chrout
 lda #' '
 jsr chrout
 ldy #$00
chgdv5
 lda $a1d0,y  ;device not present
 php
 and #$7f
 jsr chrout
 plp
 bmi chgdv6
 iny
 bne chgdv5
chgdv6
 jmp drvext
;
macmdm .byt 0
macxrg .byt 0
prmacx ;find index for macro
 cpx #3   ;from 197 f-key value
 bne prmax2
 ldx #7
prmax2 txa
 sec
 sbc #4  ;now a=0..3 for f1,3,5,7
 ldx #5
prmax3 asl a
 dex
 bpl prmax3  ;a=0,64,128,192
 sta macxrg
 rts
prtmac
 lda 197
 cmp #7
 bcc prtmac
 jsr prmacx
prtmc0
 ldx macxrg
 lda macmem,x
 beq prtmc4
 pha
 ldx macmdm
 bne prtmc2
 ldx #5
 jsr chkout
 pla
 pha
 ldx grasfl
 beq prtmc1
 jsr catosa
prtmc1
 jsr chrout
 jsr clrchn
 lda #$fd
 sta $a2
prtmcd lda $a2
 bne prtmcd
 ldx #5
 jsr chkin
 jsr getin
 cmp #$00
 bne prtmci
 ldx duplex
 beq prtmca
 ldx grasfl
 beq prtmc2
 pla
 jsr catosa
 bne prtmck
 beq prtmc3
prtmca  pla
 bne prtmc3
prtmci  tax
 pla
 txa
prtmck  ldx grasfl
 beq prtmcj
 jsr satoca
prtmcj
 pha
prtmc2
 jsr curoff
 pla
 ldx macmdm
 bne prtmcs
 jsr putbuf
prtmcs
 jsr ctrlck
 bcs prtmc3
 jsr chrout
 jsr qimoff
 jsr curprt
prtmc3  inc macxrg
 cmp #255
 bne prtmc0
prtmc4 jmp curoff
;
stbrvs .byt 0
stbcol .byt 0
stbxps .byt 0
stbyps .byt 0
stbmax .byt 0
stbmay .byt 0
cf7  ;screen to buffer
 lda #0
 sta 198
 lda #$f1
 sta $a2
scnbf0  lda $a2
 bne scnbf0
 jsr getin
 cmp #140
 bne scnbfs
 jsr bufclr
scnbfs
 lda buffoc
 pha
 lda #1
 sta buffoc
 lda #0
 sta stbrvs
 lda #255
 sta stbcol
 ldy #24
 sty stbyps
scnbf1  ldx #39
 stx stbxps
scnbf2  jsr finscp
 cmp #' '
 bne scnbf3
 dec stbxps
 bpl scnbf2
 dec stbyps
 bpl scnbf1
 jmp scnbr4
scnbf3
 lda #$0d
 jsr putbuf
 lda #$93
 jsr putbuf
 lda 53272
 and #2
 lsr a
 lsr a
 ror a
 eor #$8e
 jsr putbuf
 lda $d021
 and #15
 beq scnbnc
 tax
 lda clcode,x
 pha
 lda #2
 jsr putbuf
 pla
 jsr putbuf
scnbnc
 lda #10
 jsr putbuf
 lda stbyps
 sta stbmay
 lda #0
 sta stbyps
scnbnl
 lda #39
 sta stbxps
scnbf4
 jsr finscp
 cmp #' '
 bne scnbf5
 dec stbxps
 bpl scnbf4
 inc stbxps
 jmp scnbrt
scnbf5
 lda stbxps
 sta stbmax
 lda #0
 sta stbxps
scnbf6
 jsr finscp
 sta $02
 jsr finscc
 sta $03
 lda $02
 and #$80
 cmp stbrvs
 beq scnbf7
 lda stbrvs
 eor #$80
 sta stbrvs
 ora #18
 eor #$80
 jsr putbuf
scnbf7
 lda $02
 cmp #' '
 beq scnbf8
 lda $03
 cmp stbcol
 beq scnbf8
 tax
 lda clcode,x
 jsr putbuf
scnbf8
 lda $02
 and #$7f
 cmp #$7f
 beq scnbf9
 cmp #' '
 bcs scnb10
scnbf9
 clc
 adc #$40
 bne scnb11
scnb10
 cmp #64
 bcc scnb11
 ora #$80
scnb11
 jsr putbuf
 inc stbxps
 lda stbxps
 cmp stbmax
 bcc scnbf6
 beq scnbf6
scnbrt
 lda stbxps
 cmp #40
 bcs scnbr2
 lda #$0d
 jsr putbuf
 lda #0
 sta stbrvs
scnbr2
 inc stbyps
 lda stbyps
 cmp stbmay
 beq scnbr3
 bcs scnbre
scnbr3  jmp scnbnl
scnbre
 ldx 646
 lda clcode,x
 jsr putbuf
scnbr4
 pla
 sta buffoc
 jmp main
;
finscp
 ldy stbyps
 lda $ecf0,y
 sta locat
 lda $d9,y
 and #$7f
 sta locat+1
 ldy stbxps
 lda (locat),y
 rts
finscc
 jsr finscp
 lda locat+1
 clc
 adc #$d4
 sta locat+1
 lda (locat),y
 and #15
 rts
; .fil 5b.gs
;xmodem routines
xmstat .byt 0
xmoblk .byt 0
xmochk .byt 0
xmobad .byt 0
xmowbf .byt 0
xmodel .byt 0
xmoend .byt 0
xmostk .byt $ff
;
xmosnd
 tsx
 stx xmostk
 jsr xmoset
xmupl1
 lda #14 ;60 secs
 jsr xmmget
 beq xmupl2
xmupab jmp xmabrt
xmupl2
 cmp #can
 beq xmupab
 cmp #nak
 bne xmupl1
xmupll
 jsr xmocbf
 sty xmobad
 lda #soh
 sta (xmobuf),y
 iny
 lda xmoblk
 sta (xmobuf),y
 iny
 eor #$ff
 sta (xmobuf),y
 iny
xmsnd1
 ldx #2
 jsr chkin
xmsnd2
 jsr getin
 ldx $90  ;status
 stx xmoend
 beq xmsnd4
xmsnd3
 lda #cpmeof
xmsnd4
 sta (xmobuf),y
 clc
 adc xmochk
 sta xmochk
 iny
 cpy #131
 bcs xmsnd5
 ldx xmoend
 beq xmsnd2
 bne xmsnd3
xmsnd5
 sta (xmobuf),y
 jsr clrchn
xmsnd6
 jsr xmrclr
 ldx #5
 jsr chkout
 ldy #0
xmsnd7
 lda (xmobuf),y
 jsr chrout
 iny
 cpy #132
 bcc xmsnd7
 jsr clrchn
 jsr xmricl
 lda #3
 jsr xmmget
 bne xmsnbd
 cmp #can
 bne xmsnd8
 jmp xmabrt
xmsnd8 cmp #nak
 bne xmsnd9
xmsnbd
 jsr chrout
 jmp xmsnd6
xmsnd9 cmp #ack
 bne xmsnbd
xmsnnx
 lda #'-'
 jsr goobad
 ldx xmoend
 bne xmsnen
 inc xmoblk
 inc xmowbf
 jmp xmupll
xmsnen
 lda #0
 sta xmoend
xmsne1
 ldx #5
 jsr chkout
 lda #eot
 jsr chrout
 lda #3
 jsr xmmget
 bne xmsne2
 cmp #ack
 bne xmsne2
 jmp xmfnok
xmsne2
 inc xmoend
 lda xmoend
 cmp #10
 bcc xmsne1
 jmp xmneot
;
;
xmoset
 lda #1
 sta xmoblk
 lda #0
 sta xmowbf
 sta xmobad
xmocbf
 lda xmowbf
 and #3
 sta xmowbf
 lda #<xmoscn
 sta xmobuf
 lda #>xmoscn
 sta xmobuf+1
 ldx xmowbf
 beq xmocb2
xmocb1
 lda xmobuf
 clc
 adc #$85
 sta xmobuf
 lda xmobuf+1
 adc #0
 sta xmobuf+1
 dex
 bne xmocb1
xmocb2  ldy #0
 sty xmochk
 sty xmoend
 rts
xmrclr
 lda $029d ;clear rs232 output
 sta $029e
xmricl
 lda $029b ;and input buffers
 sta $029c
 rts
xmmget
 sta xmodel
 lda #0
 sta $a1
 sta $a2
 jsr $f04f ;"chkin" modem
xmogt1
 jsr $f14e ;rs232 input
 tax
 lda $0297
 and #8
 bne xmmgt2
 txa
 ldx #0
 rts
xmmgt2
 jsr xchkcm
 lda $a1
 cmp xmodel
 bcc xmogt1
 jsr clrchn
 and #0
 ldx #1
 rts
xincbd
 lda #':'
 jsr goobad
 inc xmobad
 lda xmobad
 cmp #10
 bcs xmtrys
 rts
xchkcm
 ldx 653
 cpx #2
 beq xmcmab
 rts
xmfnok lda #'*'
 jsr goobad
 lda #0
.byt $2c
xmabrt lda #1
.byt $2c
xmneot lda #2
.byt $2c
xmtrys lda #3
.byt $2c
xmsync lda #4
.byt $2c
xmcmab lda #5
 sta xmstat
xmoext  tsx
 cpx xmostk
 beq xmoex2
 pla
 clc
 bcc xmoext
xmoex2
 jsr xmrclr
 lda xmstat
 cmp #4
 bcc xmoex4
 ldx #5
 jsr chkout
 ldy #8
 lda #can
xmoex3
 jsr chrout
 dey
 bpl xmoex3
xmoex4
 jsr clrchn
 lda #2
 jmp close
;
xmorcv
 tsx
 stx xmostk
 jsr 61310
 jsr 61310
 jsr xmoset
 beq xmorcp
xmorc0
 jsr xincbd
xmorcp
 lda #0
 sta xmoend
xmorc1
 ldx #5
 jsr chkout
 lda #nak
 jsr chrout
 jsr clrchn
xmorcl
 lda #2
 jsr xmmget
 beq xmorc2
xmorci
 inc xmoend
 lda xmoend
 cmp #10
 bcc xmorc1
xmrcab jmp xmabrt
xmorc2
 cmp #can
 beq xmrcab
 cmp #eot
 bne xmorcs
 lda #1
 sta xmoend
 jmp xmorak
xmorcs
 cmp #soh
 bne xmorci
 jsr xmocbf
 beq xmorc4
xmorc3
 lda #2
 jsr xmmget
 bne xmorc0
xmorc4
 sta (xmobuf),y
 iny
 cpy #132
 bcc xmorc3
 ldy #1
 lda (xmobuf),y
 iny
 eor (xmobuf),y
 cmp #$ff
 bne xmorc0
 lda #0
xmorc5
 iny
 cpy #131
 bcs xmorc6
 adc (xmobuf),y
 clc
 bcc xmorc5
xmorc6
 sta xmochk
 cmp (xmobuf),y
 bne xmorc0
 ldy #1
 lda (xmobuf),y
 cmp xmoblk
 beq xmorc7
 ldx xmoblk
 dex
 txa
 cmp (xmobuf),y
 bne xmorsa
 lda #'/'
 jsr goobad
 jmp xmorc9
xmorsa  jmp xmsync
xmorc7
 jsr clrchn
 ldx #2
 jsr chkout
 ldy #3
xmorc8
 lda (xmobuf),y
 jsr chrout
 iny
 cpy #131
 bcc xmorc8
xmorc9
 lda #0
 sta xmoend
 inc xmoblk
 jsr clrchn
 lda #'-'
 jsr goobad
xmorak
 inc xmowbf
 ldx #5
 jsr chkout
 lda #ack
 jsr chrout
 jsr clrchn
 lda #0
 sta xmobad
 lda xmoend
 bne xmor10
 jmp xmorcl
xmor10
 jmp xmfnok
;
xmopsu .byt 2,'prg, ',2,'seq, or ',2,'usr? ',0
xmotyp
 lda #<xmopsu
 ldy #>xmopsu
 jsr outstr
 jsr savech
xmoty2
 jsr getin
 beq xmoty2
 and #$7f
 ldx #3
xmoty3
 cmp upltyp,x
 beq xmoty4
 dex
 bne xmoty3
 beq xmoty2
xmoty4
 stx pbuf+27
 rts
;
xmo1er .byt 13,'Transfer cancelled.',0
xmo2er .byt 13,'EOT not acknowledged.',0
xmo3er .byt 13,'Too many bad blocks!',0
xmo4er .byt 13,'Sync lost!',0
xmoupl
 jsr xmosnd
 jmp xmodon
xmodow
 jsr xmorcv
xmodon
 lda #$0d
 jsr chrout
 lda xmstat
 bne xmodn2
 jmp xfrdun
xmodn2
 cmp #5
 beq xmodna
 cmp #1
 bne xmodn3
 lda #<xmo1er
 ldy #>xmo1er
 bne xmodnp
xmodn3
 cmp #2
 bne xmodn4
 lda #<xmo2er
 ldy #>xmo2er
 bne xmodnp
xmodn4 cmp #3
 bne xmodn5
 lda #<xmo3er
 ldy #>xmo3er
 bne xmodnp
xmodn5
 lda #<xmo4er
 ldy #>xmo4er
xmodnp
 jsr outstr
 jsr gong
 lda #$0d
 jsr chrout
xmodna
 jmp abortx
xmdtxt .byt 13,13,5,'XModem ',0
xferfn
 pha
 lda protoc
 and #1
 sta protoc
 beq xferpt
 lda #<xmdtxt
 ldy #>xmdtxt
 jsr outstr
 jmp xferwc
xferpt
 lda #<ptrtxt
 ldy #>ptrtxt
 jsr outstr
xferwc
 pla
 bne xferdw
 lda #<upltxt
 ldy #>upltxt
 clc
 bcc entfnt
xferdw
 lda #<dowtxt
 ldy #>dowtxt
entfnt
 jsr outstr
 lda #<lodtxt
 ldy #>lodtxt
 jsr outstr
entfil
 lda #<flntxt
 ldy #>flntxt
 ldx #16
 jsr input
 php
 lda #$0d
 jsr chrout
 plp
 rts
abortx
 jsr clrchn
 lda #<abrtxt
 ldy #>abrtxt
 jsr outstr
 jsr coback
 lda #$02
 jsr close
 jmp main
xfermd  pha
 jmp xferm0
xfrmsg
 pha
 lda #15
 sta textcl
 sta backgr
 lda #$93
 jsr chrout
 lda #bcolor
 sta backgr
xferm0  lda #13
 sta 214
 lda #$0d
 jsr chrout
 lda #06
 sta textcl
 ldx #40
 lda #192
xferm1  jsr chrout
 dex
 bne xferm1
 lda #<xfrmed
 ldy #>xfrmed
 jsr outstr
 pla
 bne xferm2
 lda #<upltxt
 ldy #>upltxt
 clc
 bcc xferm3
xferm2
 lda #<dowtxt
 ldy #>dowtxt
xferm3
 jsr outstr
 lda #<xfrtxt
 ldy #>xfrtxt
 jsr outstr
 ldy #0
xferm4  lda inpbuf,y
 jsr chrout
 iny
 cpy max
 bne xferm4
 lda inpbuf,y
 jsr chrout
 lda inpbuf+1,y
 jsr chrout
 lda #$0d
 jsr chrout
 lda #<xf2txt
 ldy #>xf2txt
 jmp outstr
margin
 lda #<mrgtxt
 ldy #>mrgtxt
 jmp outstr
upltyp .byt 0,'p','s','u'
f1    ;upload
 jsr cosave
 lda #0
 sta mulcnt
 jsr xferfn
 bne uplfff
 jmp abortx
uplfff
 ldy max
 lda #','
 sta inpbuf,y
 lda #'p'
 sta inpbuf+1,y
 jsr filtes
 beq uplfil
 ldy max
 lda #'s'
 sta inpbuf+1,y
 jsr filtes
 beq uplfil
 ldy max
 lda #'u'
 sta inpbuf+1,y
uplmen
 jsr filtes
 beq uplfil
 pha
 ldx #$0f
 jsr chkin
 pla
 jmp drver3
uplfil
 ldy max
 ldx #03
fltpsr  lda upltyp,x
 cmp inpbuf+1,y
 beq fltpfo
 dex
 bne fltpsr
fltpfo  stx pbuf+27
 jmp uplok
filtes
 ldy max
 iny
 iny
 tya
 ldx #<inpbuf
 ldy #>inpbuf
 jsr setnam
 lda #02
 ldx diskdv
 ldy #00
 jsr setlfs
filopn  jsr open
 ldx #15
 jsr chkin
 jsr getin
 cmp #'0'
 beq filtso
 php
 pha
 lda #$02
 jsr close
 pla
 plp
filtso  rts
uplok
 lda #0
 jsr xfrmsg
 jsr clrchn
 lda protoc
 beq uplok2
 jsr margin
 jmp xmoupl
uplok2
 jsr p49173
 jsr p49164
 lda inpbuf
 cmp #01
 bne uplcon
 jsr bell
 jmp abortx
uplcon
 jsr margin
 jsr p49173
 lda #$ff
 sta pbuf+24
 jsr p49158
xfrend
 lda #02
 jsr close
 jsr clrchn
 lda #$0d
 jsr chrout
 lda mulcnt
 beq xfrnrm
 rts
xfrnrm
 lda inpbuf
 cmp #$01
 bne xfrdun
 jmp abortx
xfrdun
 lda #$0d
 jsr chrout
 jsr gong
 jmp main
f3    ;download
 lda #0
 sta mulcnt
 jsr cosave
 lda #$01
 jsr xferfn
 bne dowfok
 jmp abortx
dowfok
 lda protoc
 beq dowfo2
 jsr xmotyp
 jmp dowmen
dowfo2
 ldy max
 lda #160
 sta inpbuf,y
 sta inpbuf+1,y
dowmen  lda #01
 jsr xfrmsg
 ldx protoc
 bne dowcon
 lda inpbuf
 pha
 jsr clrchn
 jsr p49173
 jsr p49161
 ldx inpbuf
 pla
 sta inpbuf
 lda mulcnt
 bne dowcon
 cpx #01
 bne dowcon
dowabt
 jsr bell
 jmp abortx
dowcon
 ldx #$ff
 stx pbuf+24
 ldx #$0f
 jsr chkout
 lda #'i'
 jsr chrout
 lda #'0'
 jsr chrout
 lda #$0d
 jsr chrout
 jsr clrchn
 ldx #$0f
 jsr chkout
 lda #'s'
 jsr chrout
 lda #'0'
 jsr chrout
 lda #':'
 jsr chrout
 ldx #0
scrlop
 lda inpbuf,x
 jsr chrout
 inx
 cpx max
 bne scrlop
 lda #$0d
 jsr chrout
 jsr dowsfn
 lda #1
 jsr xfermd
 jsr margin
 jmp dowopn
dowsfn
 jsr clrchn
 ldx max
 lda #','
 sta inpbuf,x
 sta inpbuf+2,x
 inx
 lda #'w'
 sta inpbuf+2,x
.if v55plus
;lda protoc   ;*** 5.5 bug fix
;bne dowksp
.else
 lda protoc
 bne dowksp
.endif
 lda mulcnt
 bne dowksp
 ldy pbuf+27
 lda upltyp,y
 sta inpbuf,x
dowksp  lda max
 clc
 adc #$04
 ldx #<inpbuf
 ldy #>inpbuf
 jsr setnam
 lda #02
 ldx diskdv
 tay
 jmp setlfs
dowopn
 jsr filopn
 beq dowop2
 pha
 ldx #$0f
 jsr chkin
 pla
 jmp drver3
dowop2
 lda protoc
 beq dowop3
 jmp xmodow
dowop3
 lda #0
 sta $a2
dwwait
 lda $a2
 cmp #85
 bcc dwwait
 jsr 61310
 jsr p49173
 jsr p49155
 jmp xfrend
;
sndtxt  .byt 13,13,5,2,'read or',2,'send file? ',00
f2
 ldx 653
 cpx #02
 bne send
 jmp cf1
;send textfile
send
 jsr cosave
 lda #<sndtxt
 ldy #>sndtxt
 jsr outstr
 jsr savech
sndlop
 jsr getin
 cmp #'s'
 bne sndc1
 ldx #$40
 bne sndfil
sndc1
 cmp #'r'
 bne sndc2
 ldx #0
 beq sndfil
sndc2
 cmp #$0d
 bne sndlop
 jsr restch
 lda #$0d
 jsr chrout
sndabt
 jmp abortx
sndfil
 ora #$80
 jsr outcap
 lda #$0d
 jsr chrout
 stx bufflg
 stx buffl2
 jsr entfil
 beq sndabt
 lda #$0d
 jsr chrout
 lda max
 ldx #<inpbuf
 ldy #>inpbuf
 jsr setnam
 lda #02
 ldx diskdv
 tay
 jsr setlfs
 jsr open
 ldx #$05
 jsr chkout
 lda #15
 jsr chrout
 jsr dskout
 lda #02
 jsr close
 jsr cochng
 lda #$0d
 jsr chrout
 jmp drverr
tmsetl
 ldx #0
 stx $a2
tmloop
 ldx $a2
 cpx #$03 ;***time del
 bcc tmloop
 ldx #255
tmlop2 dex
 bne tmlop2
 rts
;
;disk output routine
dskout
 jsr clrchn
 jsr curprt
 lda bufflg  ;bufflg 00=disk
 bpl dskmo   ;$40=disk w. delay
 jsr memget  ;$80=memory get
 bit bufflg  ;$ff=mem w. delay
 bvs timdel
 ldx #$ff
mrloop
 dex
 bne mrloop
 beq chstat
dskmo
 ldx #02
 jsr chkin
 jsr getin
timdel
 bit bufflg
 bvc chstat
 jsr tmsetl
chstat
 pha
 lda status
 and #$40
 bne dskext
 jsr clrchn
 jsr curoff
 pla
 pha
 jsr ctrlck
 jsr chrout
 jsr qimoff
 ldx buffl2 ;non zero=to modem
 bne dskmo1
 pla
 jmp chkkey
dskmo1
 ldx #05
 jsr chkout
 pla
 ldx grasfl
 beq dskmo2
 jsr catosa
dskmo2
 jsr chrout
chkkey
 jsr keyprs
 beq dskout
 cmp #3
 beq dskex2
 cmp #'s'
 bne dskwat
 lda bufflg
 bpl dskwat
 jsr skpbuf
 ldx status
 bne dskex2
 jmp dskout
dskwat
 jsr keyprs
 beq dskwat
 jmp dskout
dskext
 pla
dskex2
 jsr clrchn
 jmp curoff
keyprs
 jsr clrchn
 jsr getin
 cmp #0
 rts
outstr
 sty $23
 sta $22
 ldy #0
outst1 lda ($22),y
 beq outste
 cmp #2
 beq hilite
 cmp #03
 bne outst2
 iny
 lda ($22),y
 sta 214
 lda #$0d
 jsr chrout
 lda #145
 jsr chrout
 iny
 lda ($22),y
 sta 211
 bne outst4
outst2
 cmp #$c1
 bcc outst3
 cmp #$db
 bcs outst3
 lda 53272
 and #$02
 php
 lda ($22),y
 plp
 bne outst3
 and #$7f
outst3
 jsr chrout
outst4 iny
 bne outst1
 inc $23
 bne outst1
outste rts
hilite
 lda textcl
 pha
 lda #1
 sta textcl
 lda #18  ;rvs-on
 jsr chrout
 lda #161
 jsr chrout
 lda 53272
 and #2
 php
 iny
 lda ($22),y
 plp
 beq hilit2
 ora #$80
hilit2  jsr chrout
 lda #182
 jsr chrout
 pla
 sta textcl
 lda #146
 bne outst3
;
outcap
 cmp #$c1    ;cap 'a'
 bcc outcp3
 cmp #$db    ;cap 'z'
 bcs outcp3
 pha
 lda 53272
 and #2
 beq outcp2
 pla
 bne outcp3
outcp2  pla
 and #$7f
outcp3  jmp chrout
;
xmpoly .byt 13,5,'Multi-transfer - Punter only.',13,0
cf1  ;multi-send
 jsr cosave
 lda protoc
 beq mulsav
mulnop
 lda #<xmpoly
 ldy #>xmpoly
 jsr outstr
 jmp abortx
mulsav
 lda #$93
 jsr chrout
 lda bufptr+1
 cmp #>mulfil
 bcc mulsok
 lda bufptr
 cmp #<mulfil
 bcc mulsok
 lda #<mlswrn
 ldy #>mlswrn
 jsr outstr
 jmp abortx
mulsok  lda #<msntxt
 ldy #>msntxt
 jsr outstr
 lda #<moptxt
 ldy #>moptxt
 jsr outstr
 jsr mltdir
 lda mulcnt
 bne mlss1
mlss0  jmp mlssab
mlss1
 lda mulfln
 sta mulcnt
 beq mlss0
 lda #0
 sta mulfln
 lda #<mulfil
 sta $fd
 lda #>mulfil
 sta $fe
mlslop
 ldy #19
 lda ($fd),y
 bne mlssen
mlsinc  lda $fd
 clc
 adc #20
 sta $fd
 lda $fe
 adc #0
 sta $fe
 lda $fe
 cmp #>routpt
 bcc mlslop
 jmp mulab2
mlssen
 ldy #17
mlss2  lda ($fd),y
 cmp #160
 bne mlss3
 dey
 cpy #01
 bne mlss2
 jmp mulab2
mlss3  dey
 sty max
 iny
mlss4  lda ($fd),y
 sta inpbuf-2,y
 dey
 cpy #01
 bne mlss4
 ldx max
 lda #','
 sta inpbuf,x
 ldy #18
 lda ($fd),y
 and #07
 cmp #04
 bne mlsg
 jmp mulabt
mlsg
 tay
 lda drtype,y
 sta inpbuf+1,x
mlsgo
 jsr mlshdr
 ldy #0
mlsgo1  lda inpbuf,y
 jsr chrout
 iny
 cpy max
 bne mlsgo1
 lda inpbuf,y
 jsr chrout
 lda inpbuf+1,y
 jsr chrout
 lda #$0d
 jsr chrout
 jsr clrchn
 jsr uplmen
 ldx 653
 cpx #02
 beq mulab2
 lda inpbuf
 bne mulab2
 inc mulfln
 lda mulfln
 cmp mulcnt
 beq mlss5
 ldx #00
 stx $a2
mlstim  lda $a2
 cmp #110
 bcc mlstim
 jmp mlsinc
mlss5
 jsr mlshdr
 ldx #16
 lda #04  ;ctrl-d
mlss6  jsr chrout
 dex
 bne mlss6
 lda #$0d
 jsr chrout
mlssab jsr clrchn
 jsr coback
 jsr gong
 jmp term
mlshdr  ldx #5
 jsr chkout
 ldx #16
 lda #09  ;ctrl-i
mlscri  jsr chrout
 dex
 bne mlscri
 rts
mulabt
 jsr gong
mulab2
 jsr clrchn
 lda #$0d
 jsr chrout
 lda #02
 jsr close
 jmp term
;
cf3  ;multi-receive
 jsr cosave
 lda protoc
 beq mulrav
 jmp mulnop
mulrav
 lda #01
 sta mulcnt
 lda #$93
 jsr chrout
 lda #<mrctxt
 ldy #>mrctxt
 jsr outstr
mrllgc
 ldx 653
 bne mrllgc
mlrnew
 ldy #0
 sty max
mlrwat
 ldx 653
 cpx #02
 beq mulab2
 ldx #05
 jsr chkin
 jsr getin
 cmp #09
 bne mlrwat
mlrwt2
 ldx 653
 cpx #02
 beq mulab2
 jsr getin
 cmp #0
 beq mlrwt2
 cmp #9  ;ctrl-i
 beq mlrwt2
 bne mlrfl1
mlrflp
 ldx 653
 cpx #02
 beq mulab2
 ldx #5
 jsr chkin
 jsr getin
 cmp #0
 beq mlrflp
mlrfl1   cmp #$0d
 beq mlrfl2
 ldy max
 sta inpbuf,y
 inc max
 lda max
 cmp #18
 bcc mlrflp
mlrfl2
 ldy max
 cpy #03
 bcc mlfext
 dey
 dey
 lda inpbuf,y
 cmp #','
 bne mlfext
 sty max
 lda inpbuf
 cmp #04   ;ctrl-d
 bne mlffl2
mlfext  jmp mulabt
mlffl2
 jsr dowmen
 lda inpbuf
 beq mlrnew
 bne mlfext
;
goobad
 sta 1844
 cmp #'/'
 beq goober
 cmp #'*'
 bne goob2
goober  rts
goob2 cmp #':'
 beq goob3
 ldx #3
 bne goob4
goob3  ldx #25
goob4  inc 1837,x
 lda 1837,x
 cmp #':'
 bcc goober
 lda #'0'
 sta 1837,x
 dex
 bpl goob4
 rts
;
msntxt .byt 13,14,5,18,32,'Multi-Send ',146,32,45,32
 .byt 'Select files:',13,13,0
moptxt .byt 154,32,'Yes/No/Quit/Skip8/Done/'
 .byt 'All',13,0
mrctxt .byt 13,14,5,18,32,'Multi-Receive ',13,13
 .byt 159,'Waiting for header...C= aborts.',13,0
;multi - choose files
mltdir
 lda diskdv
 jsr listen
 lda #$f0
 jsr second
 lda #'$'
 jsr ciout
 lda #'0'
 jsr ciout
 lda #':'
 jsr ciout
 lda #'*'
 jsr ciout
 jsr unlsn
 lda #<mulfil
 sta $fd
 lda #>mulfil
 sta $fe
 lda diskdv
 jsr talk
 lda #$60
 jsr tksa
 ldy #0
 sty mulcnt ;count entries
 sty mulfln
 sty mlsall
 sty mulskp
 ldy #31
mdrlp0
 jsr mgetch
 dey
 bpl mdrlp0
 ldy #$01
mdrlp1  jsr mgetch
 dey
 bpl mdrlp1
 ldy #0
 jsr mgetch
 sta ($fd),y
 sta $07e8,y
 iny
 jsr mgetch
 sta ($fd),y
 sta $07e8,y
 lda #0
 sta $06
mdrlp2  jsr mgetch
 inc $06
 cmp #'"'
 bne mdrlp2
mdrlpf
 iny
 cpy #18
 beq drlpfn
 jsr mgetch
 cmp #'"'
 bne drlpnq
 lda #160
drlpnq
 sta ($fd),y
 sta $07e8,y
 jmp mdrlpf
drlpfn
 dey
 cpy #01
 beq drlptc
 lda $07e8,y
 cmp #' '
 bne drlptc
 lda #160
 sta ($fd),y
 sta $07e8,y
 bne drlpfn
drlptc
 jsr mgetch
 lda #00
 sta $05
 jsr mgetch
 cmp #'*'
 bne drlpsp
 lda #$80
 sta $05
drlpsp
 jsr mgetch
 ldx #04
drlptl
 cmp drtype,x
 beq drlptp
 dex
 bne drlptl
drlptp
 txa
 ora $05
 sta $05
 jsr mgetch
 jsr mgetch
 jsr mgetch
 cmp #'<'
 bne drlpte
 lda $05
 ora #$40
 sta $05
drlpte  lda $05
 ldy #18
 sta ($fd),y
 sta $07e8,y
 lda #00
 iny
 sta ($fd),y
dirgrb
 jsr mgetch
 bne dirgrb
 inc mulcnt
 lda mulskp
 bne mulpmt
 jsr mdrret
 bne mulnen
mulpmt dec mulskp
 jsr drpol7
mulnen
 lda diskdv
 jsr talk
 lda #$60
 jsr tksa
 ldy #01
 jmp mdrlp1
mgetch  jsr acptr
 ldx status
 bne mdrlp3
 cmp #00
 rts
mdrlp3  pla
 pla
mdrext  lda diskdv
 jsr listen
 lda #$e0
 jsr second
 jsr untlk
 jsr unlsn
 jmp clrchn
mdrret
 ldy #0
drpol0
 sty $02
 lda drform,y
 cmp #02   ;ctrl-b
 bne drpol1
 ldy #00
 lda $07e8,y
 tax
 iny
 lda $07e8,y
 jsr $bdcd
 ldy $06
drprbl
 lda #' '
 jsr chrout
 dey
 bne drprbl
 beq drpol4
drpol1
 cmp #$0e  ;ctrl-n
 bne drpol2
 ldy #02
drprnm
 lda $07e8,y
 jsr chrout
 iny
 cpy #18
 bne drprnm
 beq drpol4
drpol2
 cmp #$06  ;ctrl-f
 bne drpol3
 ldy #18
 lda $07e8,y
 tay
 and #07
 tax
 tya
 and #$80
 bne drprf1
 lda #' '
 bne drprf2
drprf1  lda #'*'
drprf2  jsr chrout
 lda drtype,x
 jsr chrout
 lda drtyp2,x
 jsr chrout
 lda drtyp3,x
 jsr chrout
 tya
 and #$40
 bne drprf3
 lda #' '
 bne drprf4
drprf3  lda #'<'
drprf4  jsr chrout
 bne drpol4
drpol3
 jsr chrout
drpol4
 ldy $02
 iny
 cpy #14
 beq drpol5
 jmp drpol0
drpol5
 lda mlsall
 beq mlsf0
 lda #'y'
 jsr chrout
 bne mlsyes
mlsf0
 lda #' '
 jsr chrout
 lda #$9d
 jsr chrout
 jsr curprt
mlswlp  jsr getin
 beq mlswlp
 and #127
 cmp #'a'
 bcc mlswlp
 cmp #'['
 bcs mlswlp
 pha
 jsr curoff
 pla
 pha
 jsr chrout
 lda #$9d
 jsr chrout
 pla
 cmp #'y'
 bne mlsf1
mlsyes  ldy #19
 inc mulfln
 lda #$80
 sta ($fd),y
 bne mlsnpr
mlsf1  cmp #'n'
 beq mlsnpr
 cmp #'a'
 bne mlsf2
 lda #01
 sta mlsall
 bne mlsyes
mlsf2
 cmp #'d'
 bne mlsf3
 lda #$0d
 jsr chrout
 jmp mdrlp3
mlsf3
 cmp #'q'
 bne mlsf4
 jsr mdrext
 pla
 pla
 pla
 pla
 jsr clrchn
 jmp term
mlsf4
 cmp #'s'
 bne mlsf0
 lda #07
 sta mulskp
mlsnpr  lda #$0d
 jsr chrout
drpol7
 lda $fd
 clc
 adc #20
 sta $fd
 lda $fe
 adc #0
 sta $fe
 rts
;
buftxt .byt 5,'Buffer ',00
buftx2 .byt ' bytes free.      ',13,2,'open  ',2,'close  ',2,'erase  '
.byt 2,'transfer',13,2,'load  ',2,'save   ',2,'print  ',2,'view: ',0
opntxt .byt 'Open  ',00
clotxt .byt 'Closed',00
erstxt .byt 'Erase Buffer! - ',2,'yes or ',2,'no?    ',157,157,157,0
snbtxt .byt 13,13,'Sending Buffer...',13,13,00
dontxt .byt 13,13,5,'Done.',13,0
bufmsg
 lda #<buftxt
 ldy #>buftxt
 jsr outstr
 lda buffoc
 beq bufms1
 lda #<opntxt
 ldy #>opntxt
 clc
 bcc bufms2
bufms1
 lda #<clotxt
 ldy #>clotxt
bufms2
 jmp outstr
bufprm
 lda #$0d
 jsr chrout
 jsr bufmsg
 lda #' '
 jsr chrout
 lda #'-'
 jsr chrout
 lda #' '
 jsr chrout
 lda #<bufend
 sec
 sbc bufptr
 tax
 lda #>bufend
 sbc bufptr+1
 jsr outnum
 lda #<buftx2
 ldy #>buftx2
 jmp outstr
f4
 ldx 653
 cpx #02
 bne buffrc
 jmp cf3
buffrc  ;buffer cmds
 jsr cosave
 lda #0
 sta visaut
 lda #13
 sta f7mtxt+1
 sta f7mtx1
 lda #10
 sta ttdtxt+1
 sta ttptxt+1
 sta ttntxt+1
 lda #3
 sta f7thob
bufask
 lda #$0d
 jsr chrout
 jsr bufprm
bufwat
 jsr savech
buflop
 jsr getin
 beq buflop
 and #127
 pha
 jsr restch
 pla
 cmp #$0d
 bne bufcmd
bufext
 lda #' '
 jsr chrout
 lda #$0d
 jsr chrout
 jsr chrout
 jsr coback
 jmp main
bufcmd
 cmp #'o'
 bne bufcm2
 ldx #$01
 stx buffoc
 bne bufex1
bufcm2
 cmp #'c'
 bne bufcm3
 ldx #0
 stx buffoc
bufex1
 ora #$80
bufexa
 jsr outcap
 lda #$0d
 jsr chrout
 lda #145 ;crsr up
 ldx #04
bufex2
 jsr chrout
 dex
 bpl bufex2
 jmp bufask
bufcm3
 cmp #'e'
 bne bufcm4
 ora #$80
 jsr outcap
 lda #$0d
 jsr chrout
 lda #145
 ldx #02
bufer1
 jsr chrout
 dex
 bpl bufer1
 lda #<erstxt
 ldy #>erstxt
 jsr outstr
 jsr savech
bufer2
 jsr getin
 beq bufer2
 and #127
 cmp #'n'
 beq bufer3
 cmp #'y'
 bne bufer2
 jsr bufclr
bufer3
 jsr restch
 lda #145
 jsr chrout
 jsr chrout
 jmp bufask
bufcm4
 cmp #'p'
 bne bufvew
 ora #$80
 jsr outcap
 jmp bufpro
bufvew
 cmp #'v'
 bne bufcm5
 lda #$93
 jsr chrout
 lda #$80
 sta bufflg
 and #0
 sta buffl2
 jsr prtbuf
 jmp main
prtbuf ;buf.to screen
 lda buffst
 pha
 lda buffst+1
 pha
 lda #$2f
 sta $00
 lda #$36
 sta $01
 jsr dskout
 lda #$37
 sta $01
 pla
 sta buffst+1
 pla
 sta buffst
 rts
memget
 ldx buffst
 cpx bufptr
 bcc memok
 ldx buffst+1
 cpx bufptr+1
 bcc memok
memgab ldx #$40
 stx status
 rts
memok
 ldy #0
 lda (buffst),y
 inc buffst
 bne memext
 inc buffst+1
memext
 ldx #0
 stx status
 rts
skpbuf
 lda buffst+1
 cmp bufptr+1
 bcs memgab
 inc buffst+1
skpbf2
 lda buffst+1
 cmp bufptr+1
 bcc memext
 lda buffst
 cmp bufptr
 bcs memgab
 bcc memext
;
bufcm5
 cmp #'s'
 bne bufcm6
 jsr solfil
 jmp savbuf
solfil
 ora #$80
 jsr outcap
 lda #$0d
 jsr chrout
 jsr chrout
 jsr entfil
 bne solfok
 jmp abortx
solfok  rts
savbuf
 lda #0
 sta mulcnt
 lda #$02
 sta pbuf+27
 jsr dowsfn
 lda #$36
 sta $01
 lda buffst
 sta $c1
 lda buffst+1
 sta $c2
 lda bufptr
 clc
 adc #$01
 sta $ae
 lda bufptr+1
 adc #0
 sta $af
 lda #$61
 sta $b9
 jsr $f3d5
 jsr $f68f
 lda $ba
 jsr $ed0c
 lda $b9
 jsr $edb9
 ldy #0
 jsr $fb8e
 jsr $f624
 php
 lda #$37
 sta $01
 plp
 bcc bsaved
 lda #$0d
 jsr chrout
 jsr bell
 jmp abortx
bsaved
 jmp bufext
bufcm6
 cmp #'l'
 bne bufcm7
 jsr solfil
lodbuf
 lda #$02
 ldx diskdv
 tay
 jsr setlfs
 lda max
 ldx #<inpbuf
 ldy #>inpbuf
 jsr setnam
 jsr open
 ldx #$02
 jsr chkin
lodbfl
 jsr getin
 ldx status
 bne lodbex
 ldx bufptr
 cpx #<bufend
 bne lodbok
 ldx bufptr+1
 cpx #>bufend
 beq lodbex
lodbok
 ldy #0
 sta (bufptr),y
 inc bufptr
 bne lodbfl
 inc bufptr+1
 bne lodbfl
lodbex
 jsr clrchn
 lda #$02
 jsr close
 jmp bufext
bufcm7
 cmp #'t'
 beq sndbuf
 cmp #'<'
 beq bufchg
 cmp #'>'
 bne bufbak
bufchg
 jsr chgbpr
 jmp bufexa
bufbak
 jmp bufwat
sndbuf
 ora #$80
 jsr outcap
 lda #<snbtxt
 ldy #>snbtxt
 jsr outstr
 lda #$ff
 sta bufflg
 sta buffl2
 jsr prtbuf
 jsr cosave
 lda $029b
 sta $029c
 lda #<dontxt
 ldy #>dontxt
 jsr outstr
 jsr coback
 jmp main
chgbpr
 pha
 cmp #'>'
 beq chgbp3
 lda bufptr+1
 cmp #>endprg
 bne chgbp1
 lda bufptr
 cmp #<endprg
 beq chgben
chgbp1
 lda bufptr
 bne chgbp2
 dec bufptr+1
chgbp2  dec bufptr
 jmp chgben
chgbp3
 lda bufptr+1
 cmp #>bufend
 bne chgbp4
 lda bufptr
 cmp #<bufend
 beq chgben
chgbp4
 inc bufptr
 bne chgben
 inc bufptr+1
chgben
 ldx #1
 stx 651
 pla
 rts
;
bufpdt .byt 13,13,'Device: ',0
bufpda .byt 13,'Sec.A.: ',0
bufpdp .byt $93,13,'Printing...',13,0
bufpro
 lda #<bufpdt
 ldy #>bufpdt
 ldx #1
 jsr inpset
 lda #'4'
 jsr chrout
 jsr inputl
 bne bufpr2
bufpra lda #$0d
 jsr chrout
 jmp abortx
bufpr2 lda inpbuf
 cmp #'3'
 bcc bufpra
 cmp #'6'
 bcs bufpra
 and #$0f
 pha
 lda #<bufpda
 ldy #>bufpda
 ldx #1
 jsr inpset
 lda #'7'
 jsr chrout
 jsr inputl
 beq bufpra
 lda inpbuf
 cmp #'0'
 bcc bufpra
 cmp #':'
 bcs bufpra
 and #$0f
 tay
 pla
 tax
 lda #4
 jsr setlfs
 lda #0
 jsr setnam
 lda #<bufpdp
 ldy #>bufpdp
 jsr outstr
 jsr open
 ldx status
 bne bufpr3
 lda buffst
 pha
 lda buffst+1
 pha
 lda #$2f
 sta $00
 lda #$36
 sta $01
 jsr mempro
 lda #$37
 sta $01
 pla
 sta buffst+1
 pla
 sta buffst
bufpr3
 lda #4
 jsr close
 lda #<dontxt
 ldy #>dontxt
 jsr outstr
 jsr coback
 jmp main
mempro
mempr2
 jsr memget
 bne mempr3
 pha
 and #$7f
 cmp #$0d
 beq memprp
 cmp #$20
 bcc mempab
memprp
 ldx #4
 jsr chkout
 pla
 jsr chrout
 ldx status
 bne mempr3
 jmp mempr2
mempab pla
 jmp mempr2
mempr3
 jmp clrchn
;
entcol .byt 5
hilcol .byt 158
phhtxt
.byt 19,13
.byt 5,18,161,'CRSR keys',182,146,154,' - ','Move '
.byt 5,18,161,'RETURN',182,146,154,' - ','Select',13
.byt 159,2,'dial Unlisted #  ',2,'edit Current #',13
.byt 2,'call Current #   ',2,'a-','Dial All Select',29,20
.byt 'd',157,148,'e',13,2,'reverse All      ',2,'x-Return to '
.byt 'Menu',13
.byt 152,3,5,0,18,'           >>>','Phone Book<<<           '
.byt 29,20,32,157,148,32,13,0
stattx .byt 152,3,21,0,18,'                                      ',29,20
.byt 32,157,148,32,13,145,18,0
staptx .byt 152,3,21,0,18,32,0
hp1msg .byt 'Use +/-/RETURN to select:',0
toetxt .byt 3,6,0,0
curbtx .byt 3,22,1,159,'Name:',13,'Phone:',13,32,'Baud: ',29,29,29,29
.byt '  Term: ',29,29,29,29,29,29,29,29,'  Try: ',29
curbt2 .byt 29,20,29,20,145,13,0
gfxbbs .byt 'Graphics',0
ascbbs .byt 'ASCII   ',0
nontxt .byt 5,'(None)             ',13,0
clrlnt .byt 3,22,7,'                  ',3,22,7,5,0
empbbs .byt 151,164,164,164,164,164,164,164,164,164,164,164,164
 .byt 164,164,164,164,164,164
curbbs .byt 146
colbbs .byt 153
nambbs .byt '                ',146,5,0
curpik .byt 0
tmppik .byt 0
bautmp .byt 6
gratmp .byt 0
prtstt
 pha
 tya
 pha
 lda #<staptx
 ldy #>staptx
 jsr outstr
 pla
 tay
 pla
 jsr outstr
 lda #' '
prtst2  ldx 211
 cpx #39
 bcs prtst3
 jsr chrout
 bne prtst2
prtst3  rts
phnptr
 lda curpik
 sta nlocat
 lda #52
 sta nlocat+1
 jsr multpy
 jmp phnpt4
multpy  clc
 lda #0
 ldx #$08
phnpt2  ror a
 ror nlocat
 bcc phnpt3
 clc
 adc nlocat+1
phnpt3  dex
 bpl phnpt2
 sta nlocat+1
 rts
phnpt4
 lda nlocat
 clc
 adc #<phbmem
 sta nlocat
 lda nlocat+1
 adc #>phbmem
 sta nlocat+1
 ldy #0
 rts
onpent
 lda hilcol
 bne prten0
prtent
 lda entcol
prten0  sta colbbs
prten1  lda #146
 sta curbbs
 ldy #0
 lda (nlocat),y
 beq prtcur
 lda #18
 sta curbbs
prtcur   ldy #2
prten2   lda (nlocat),y
 sta nambbs-2,y
 iny
 cpy #20
 bcc prten2
 lda nambbs
 bne prten4
 ldy #1
prten3  lda empbbs,y
 sta colbbs,y
 iny
 cpy #19
 bcc prten3
 lda colbbs
 cmp hilcol
 beq prten4
 lda empbbs
 sta colbbs
prten4
 ldy #0
prten5  lda curbbs,y
 beq prten6
 jsr chrout
 iny
 bne prten5
prten6  lda #$0d
 jmp chrout
;
clrent
 lda #0
 sta curpik
clren1
 jsr phnptr
 lda #0
 sta (nlocat),y
 inc curpik
 lda curpik
 cmp #30
 bcc clren1
 rts
;
phinit
 lda #0
 sta trycnt
 sta curpik
 jsr clrchn
 lda #<phhtxt
 ldy #>phhtxt
 jsr outstr
 lda #<toetxt
 ldy #>toetxt
 jsr outstr
phini2
 lda #29
 jsr chrout
 jsr phnptr
 jsr prtent
 inc curpik
 lda curpik
 cmp #15
 bcc phini2
 lda #<toetxt
 ldy #>toetxt
 jsr outstr
phini3  lda #21  ;col 21
 sta 211
 jsr phnptr
 jsr prtent
 inc curpik
 lda curpik
 cmp #30
 bcc phini3
 lda #<stattx
 ldy #>stattx
 jsr outstr
 lda #0
 sta curpik
 lda #<curbtx
 ldy #>curbtx
 jmp outstr
phnroc .byt 3,0,0,0
arrowt .byt 32,93,93,32,60,125,109,62,32,32,0
hilcur
 ldx curpik
 inx
 txa
 and #$0f
 clc
 adc #5
 sta phnroc+1  ;row
 lda #1
 sta phnroc+2  ;col
 lda curpik
 cmp #15
 bcc hilcu2
 inc phnroc+1
 lda #21
 sta phnroc+2
hilcu2
 lda colbbs
 cmp hilcol
 bne hilcu7
 ldx toetxt+1
hilcu3
 lda $ecf0,x
 sta nlocat
 lda $d9,x
 and #$7f
 sta nlocat+1
 ldy #0
 cpx phnroc+1
 bne hilcu4
 ldy #4
 bne hilcu5
hilcu4
 bcc hilcu5
 ldy #8
 bne hilcu6
hilcu5
 lda phnroc+2
 cmp #20
 bcc hilcu6
 iny
 iny
hilcu6
 lda arrowt,y
 pha
 lda arrowt+1,y
 ldy #20
 sta (nlocat),y
 pla
 dey
 sta (nlocat),y
 lda nlocat+1
 clc
 adc #212
 sta nlocat+1
 lda #5  ;green
 sta (nlocat),y
 iny
 sta (nlocat),y
 inx
 cpx #21
 bcc hilcu3
hilcu7
 lda #<phnroc
 ldy #>phnroc
 jsr outstr
 jsr phnptr
 jmp prten1
posnam
 ldx curbtx+1
 dex
 stx 214
 lda #$0d
 jsr chrout
 lda #7  ;start at col 7
 sta 211
 rts
;;; .fil 5c.gs
shocol .byt 7,7
shocur
 jsr posnam
 lda #5
 sta colbbs
 lda #146
 sta curbbs
 ldy #2
 lda (nlocat),y
 bne shocrp
 lda #<nontxt
 ldy #>nontxt
 jsr outstr
 jmp shocr0
shocrp jsr prtcur
shocr0 lda #7
 sta 211
 ldy #20
shocr1 lda (nlocat),y
 beq shocr2
 jsr chrout
 iny
 cpy #52
 bcc shocr1
shocr2  lda #' '
 ldx 211
 cpx #39
 bcs shocr3
 jsr chrout
 bne shocr2
shocr3
 lda #7
 sta shocol
 sta shocol+1
shobau
 lda #23
 sta 214
 lda #$0d
 jsr chrout
 lda #7
 sta 211
 lda shocol
 sta 646
 ldy #1
 lda (nlocat),y
 and #7
 asl a
 tay
 ldx bpsspd,y
 lda bpsspd+1,y
 jsr outnum
 lda #' '
 jsr chrout
 lda #19
 sta 211
 lda shocol+1
 sta 646
 ldy #1
 lda (nlocat),y
 and #$80
 bne shasct
 lda #<gfxbbs
 ldy #>gfxbbs
 bne shotty
shasct
 lda #<ascbbs
 ldy #>ascbbs
shotty
 jsr outstr
 lda #34
 sta 211
 lda #7
 sta 646
 ldx trycnt
 lda #0
 jsr outnum
 lda #<curbt2
 ldy #>curbt2
 jsr outstr
 lda #19
 jmp chrout
;
xorall
 lda #0
 sta curpik
xoral2
 jsr xorent
 inc curpik
 lda curpik
 cmp #30
 bcc xoral2
 rts
xorent
 jsr phnptr
 ldy #2
 lda (nlocat),y
 bne xortog
xorabt rts
xortog
 ldy #0
 lda (nlocat),y
 eor #$01
 sta (nlocat),y
 rts
;
newent
 jsr posnam
 ldx #18
 lda #0
 jsr inpset
 ldy #2
 lda (nlocat),y
 bne newen2
 dey
 lda bautmp
 sta (nlocat),y
 lda gratmp
 lsr a
 ror a
 ora (nlocat),y
 sta (nlocat),y
 lda #<clrlnt
 ldy #>clrlnt
 jsr outstr
 jmp newen4
newen2
 ldy #17
newenl lda nambbs,y
 cmp #' '
 bne newen3
 dey
 bpl newenl
newen3 iny
 tya
 clc
 adc 211
 sta 211
newen4
 lda #1
 sta 646
 jsr inputl
 lda #0
 sta inpbuf,x
 cpx #0
 bne neweok
newugh jmp zerent
neweok
 lda inpbuf
 cmp #' '
 beq newugh
 ldy #19
 lda #' '
newen5  sta (nlocat),y
 dey
 cpy #1
 bne newen5
 iny
 lda inpbuf
 sta (nlocat),y
 ldx #0
 ldy #2
newen6 lda inpbuf,x
 beq newen7
 sta (nlocat),y
 iny
 inx
 cpx #18
 bcc newen6
newen7
 lda #$0d
 jsr chrout
 lda #7
 sta 211
 lda #0
 ldx #32
 jsr inpset
 ldy #20
newen8 lda (nlocat),y
 beq newen9
 iny
 cpy #52
 bcc newen8
newen9 tya
 sec
 sbc #20
 clc
 adc 211
 sta 211
 jsr inputl
 lda #0
 sta inpbuf,x
 cpx #0
 bne newpok
 ldy #2
 sta (nlocat),y
 jmp newent
newpok
 tax
 ldy #20
newena  lda inpbuf,x
 sta (nlocat),y
 beq newenb
 iny
 inx
 cpy #52
 bcc newena
newenb
 lda #<hp1msg
 ldy #>hp1msg
 jsr prtstt
 lda #7
 sta tmpmax
 ldy #1
 lda (nlocat),y
 and #7
 sta tmpopt
newenc
 lda #1
 sta shocol
 lda #7
 sta shocol+1
 jsr shobau
 jsr newsel
 bcc newene
 ldy #1
 lda (nlocat),y
 and #$80
 ora tmpopt
 sta (nlocat),y
 jmp newenc
newene
 lda #2
 sta tmpmax
 ldy #1
 lda (nlocat),y
 and #$80
 asl a
 rol a
 sta tmpopt
newenf
 lda #7
 sta shocol
 lda #1
 sta shocol+1
 jsr shobau
 jsr newsel
 bcc neweng
 ldy #1
 lda tmpopt
 lsr a
 ror a
 and #$80
 sta tmptmp
 lda (nlocat),y
 and #7
 ora tmptmp
 sta (nlocat),y
 jmp newenf
neweng
 lda #<stattx
 ldy #>stattx
 jmp outstr
zerent
 ldy #51
 lda #0
zeren2  sta (nlocat),y
 dey
 bpl zeren2
 rts
;
tmpopt .byt 00
tmpmax .byt 00
tmptmp .byt 00
newsel
 jsr getin
 cmp #'+'
 bne newsl2
 inc tmpopt
 lda tmpopt
 cmp tmpmax
 bcc newsl1
 lda #0
 sta tmpmax
newsl1 sec
 rts
newsl2 cmp #'-'
 bne newsl3
 dec tmpopt
 bpl newsl1
 ldx tmpmax
 dex
 stx tmpopt
 sec
 rts
newsl3 cmp #$0d
 bne newsel
 clc
 rts
;
phbook
 lda #$93
 jsr chrout
 jsr phinit
phloop
 lda #0
 sta trycnt
 lda bautmp
 sta baudrt
 lda gratmp
 sta grasfl
 lda hilcol
 sta colbbs
 jsr hilcur
 jsr shocur
phbget
 jsr getin
 cmp #$00
 beq phbget
 cmp #157  ;left
 bne phb2
 lda curpik
 sbc #15
 bcs phnupd
 adc #30
 jmp phnupd
phb2  cmp #29 ;right
 bne phb3
 lda curpik
 clc
 adc #15
 cmp #30
 bcc phnupd
 sbc #30
 jmp phnupd
phb3  cmp #145 ;up
 bne phb4
 lda curpik
 sbc #1
 bcs phnupd
 adc #30
 jmp phnupd
phb4  cmp #17  ;down
 bne phb5
 lda curpik
 clc
 adc #1
 cmp #30
 bcc phnupd
 sbc #30
phnupd
 pha
 lda entcol
 sta colbbs
 jsr hilcur
 pla
 sta curpik
 jmp phloop
phb5
 cmp #19
 bne phb6
phbhom  lda #0
 beq phnupd
phb6
 cmp #$93
 bne phb7
 jsr clrent
 jsr phinit
 jmp phloop
phb7
 and #$7f
 cmp #'x'
 bne phb8
 jsr baud
 jmp f7
phb8
 cmp #' '
 beq phnsel
 cmp #$0d
 bne phb9
phnsel  ldy #2
 lda (nlocat),y
 bne phntog
phabrt jmp phbget
phntog
 ldy #0
 lda (nlocat),y
 eor #$01
 sta (nlocat),y
 jmp phloop
phb9  cmp #'r'
 bne phb10
 jsr xorall
 jsr phinit
 jmp phloop
phb10
 cmp #'e'
 bne phb11
 jsr newent
 jmp phloop
phb11
 cmp #'c'
 bne phb12
 jmp dialts
phb12
 cmp #'a'
 bne phb13
 jmp dalsel
phb13
 cmp #'d'
 bne phb14
 jmp dalunl
phb14
 jmp phbget
;
dialts
 lda #0
 sta daltyp
 lda #<calctx
 ldy #>calctx
 jsr prtstt
;
dialcr
 jsr phnptr
 ldy #20
dialc1 lda (nlocat),y
 beq dialc2
 sta numbuf-20,y
 iny
 cpy #52
 bcc dialc1
dialc2
 lda #$0d
 sta numbuf-20,y
 lda numbuf
 cmp #$0d
 bne dialc3
 lda #0
 sta whahap
 jmp dalfin
dialc3
 ldy #1
 lda (nlocat),y
 and #7
 sta baudrt
 jsr baud
 ldy #1
 lda (nlocat),y
 and #$80
 asl a
 rol a
 sta grasfl
 jmp dial
;
dalfin
 lda whahap
 cmp #1
 bne dalf2    ;connected
 lda #<conntx
 ldy #>conntx
 jsr prtstt
 lda #$e0
 sta $a2
dalfcl  lda $a2
 bne dalfcl
 lda #$0f
 sta $d418
 lda trycnt
 cmp #4
 bcc dalfc1
 jsr gong
 jmp term
dalfc1 jsr bell
 jmp term
dalf2
 cmp #2      ;aborted
 bne dalf3
 jmp dalfab
dalf3
 cmp #0
 bne dalf4
 lda daltyp  ;no connect
 cmp #2
 bcs dalslc
 lda numbuf
 cmp #$0d
 bne dalag
 jmp dlabrt
dalag
 jmp adnum   ;redial for curr/unl
dalslc
 lda motype
 cmp #5
 bcs dalsl0
 lda #<stattx
 ldy #>stattx
 jsr outstr
 jmp dalsl0
dalsel  ;dial selected
 lda #0
 sta trycnt
 lda #<dalstx
 ldy #>dalstx
 jsr prtstt
dalsl0
 lda #2
 sta daltyp
 lda curpik
 sta tmppik
 lda entcol
 sta colbbs
 jsr hilcur
 lda trycnt
 beq dalsl3
dalsl1
 inc curpik
 lda curpik
 cmp #30
 bcc dalsl2
 lda #0
 sta curpik
dalsl2
 cmp tmppik
 bne dalsl3
 jmp dlabrt
dalsl3
 jsr phnptr
 ldy #0
 lda (nlocat),y
 beq dalsl1
 lda hilcol
 sta colbbs
 jsr hilcur
 jsr shocur
 jmp dialcr
dalf4  ;1660-result indeterminate
 jmp rdtbeg
;
dalfab
 lda #<stattx
 ldy #>stattx
 jsr outstr
 jmp phloop
defstd .byt 0
defstf .byt 6
;
dalunl
 lda #1
 sta daltyp
 lda entcol
 sta colbbs
 jsr hilcur
 lda #<dulstx
 ldy #>dulstx
 jsr prtstt
 lda grasfl
 beq dalun1
 lda #$80
dalun1  sta defstf
 lda baudrt
 ora defstf
 sta defstf
 lda #<defstd
 sta nlocat
 lda #>defstd
 sta nlocat+1
 jsr shocr3
 lda #<clrlnt
 ldy #>clrlnt
 jsr outstr
 lda #<unlstx
 ldy #>unlstx
 jsr outstr
 ldy #31
 lda #' '
dalun2 sta 1951,y
 dey
 bpl dalun2
 lda #7
 sta 211
 and #0
 ldx #32
 jsr input
 bne dalun3
 jmp dlabrt
dalun3
 lda #$0d
 sta inpbuf,x
dalun4  lda inpbuf,x
 sta numbuf,x
 dex
 bpl dalun4
 jmp dial
;
calctx .byt 'Call Current Number...',0
dalstx .byt 'Dial Selected Numbers...',0
dulstx .byt 'Dial Unlisted Number.',0
unlstx .byt 'Unlisted.',13,0
daltxt .byt 'Dialing...',0
wcrtxt .byt 'Waiting for carrier...',0
pabtxt .byt 'Dialing...  Press STOP to abort.',0
rdttxt .byt 32,'Redial, Terminal, or Abort? ',0
numbuf
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
numptr .byt 0
ttflag .byt 0
numtmp .byt 0
trycnt .byt 0 ;how many tries?
daltyp .byt 0 ;0=curr, 1=unlisted
              ;2=selected
whahap .byt 0 ;status after call
;0=busy/no carrier, 1=connect
;2=aborted w/stop , 3=dunno(1660)
;
;main body of dialer
dial
adnum
 lda datdir    ;set up modem here
 ora #$20
 sta datdir
 lda mopo2     ;hang up modem
 sta modreg
;
 lda #$0d
 jsr chrout
 lda motype
 cmp #$05
 bcs await0
await0
 lda #$96      ;1.75 sec delay
 sta $a2
await1
 jsr getin    ;check r/s
 cmp #$03
 bne awaitl
 jmp dlabrt
awaitl
 lda $a2
 bne await1
adbegn
 lda mopo1      ;pick up phone
 sta modreg
 lda #$88       ;2 sec delay
 sta $a2        ;for dial tone
await2
 lda $a2
 bne await2
 inc trycnt
 jsr shocr3
dialin
 ldy #31
 lda #1
dlwhtl sta 56223,y
 dey
 bpl dlwhtl
 lda motype
 cmp #$04
 bcs dialn2
 cmp #$02
 bcc dialn2
 lda tonpul
 beq dialn2
 lda #$0f
 sta $d418
 lda #$10
 sta $d404
 sta $d40b
 lda #$00
 sta $d405
 sta $d40c
 sta $d417
 lda #$f0
 sta $d406
 sta $d40d
 bne dlinit
dialn2
 lda #<pabtxt  ;print stop aborts
 ldy #>pabtxt  ;if not tone dial
 jsr prtstt
 jmp dlini2
dlinit
 lda #<daltxt  ;print 'dialing:
 ldy #>daltxt
 jsr prtstt
dlini2
 ldx #$00
 stx numptr
 lda motype
 cmp #$05
 bcc dlgetn
 jmp smrtdl
dlgetn
 ldx numptr
 lda numbuf,x  ;get number
 cmp #$0d
 bne dlprnt
 jmp dlend
dlprnt
 inx
 stx numptr    ;store x pointer
 pha
 lda #14  ;lt. blue
 sta 56222,x
 pla
 cmp #'-'
 bne dldash
 jmp dlgetn
dldash
 jsr caldig
 sta numtmp
 cmp #$00
 bne dltpc
 lda #$b0
 sta $a2
dlpaus         ;pause char then
 lda $a2       ;next digit
 bne dlpaus
 jmp dlgetn
dltpc
 ldx tonpul    ;tone or pulse?
 beq plsdal    ;(if tone, must be
 ldx motype    ; a 1660)
 cpx #$02
 bcc plsdal
 cpx #$04
 bcs plsdal
 jmp tonedl
plsdal
 cmp #$0b
 bcc dldigt
 jmp dlgetn
dldigt
 lda #$00
 sta tonpul
 lda mopo4     ;set up modem
 sta modreg    ;for click toggle
 lda #$fd
 sta $a2
dlwat1         ;2/60 sec delay
 lda $a2
 bne dlwat1
 lda mopo3     ;click toggle
 sta modreg
 lda #$fd
 sta $a2
dlwat2         ;2/60 sec delay
 lda $a2
 bne dlwat2
 dec numtmp    ;decrement number
 bne dldigt    ;through yet?
 lda #$f0
 sta $a2
dlwat3         ;if so, 1/6 sec
 lda $a2       ;delay.
 bne dlwat3
;
 jsr getin     ;check for r/s
 cmp #$03      ;key to abort
 bne dlcont
 jmp dlabrt
dlcont
 jmp dlgetn     ;get next number
dlend
 lda #$0d
 jsr chrout
 lda motype
 cmp #$02       ;don't wait for
 bne carrwt     ;carrier if 1660
 jmp rt1660     ;w/o car.det.
carrwt
 lda #<wcrtxt   ;print 'waiting..
 ldy #>wcrtxt
 jsr prtstt
dllstn
 lda #$00
 sta $a1
 sta $a2
crwait
 lda modreg      ;check for dcd
 and #$10
 beq cargot
 jsr getin
 cmp #$03
 beq dlabrt
 lda $a1
 cmp #$01
 bne crwait
 lda $a2
 cmp #180
 bcc crwait
carred
 jmp redial     ;go to dial again
cargot
 lda motype
 cmp #$04       ;if 1064, tell
 bne cargt2     ;it to send a
 lda #$ff       ;carrier
 sta modreg
cargt2
 lda $a2
 clc
 adc #$84
 sta $a3
carchk
 lda modreg     ;check for dcd
 and #$10       ;again
 bne crwait
 lda $a2
 cmp $a3
 bne carchk
 lda #1         ;got carrier!
 sta whahap     ;set flag
 jmp dalfin
dlabrt
 lda #$20       ;aborted w/stop!
 sta datdir
 lda mopo2      ;hang up modem
 sta modreg
 lda #$d0
 sta $a2        ;short delay
dlablp
 lda $a2
 bne dlablp     ;back to phbook
dgobak
 lda #2
 sta whahap
 jmp dalfin
redial
 lda mopo2      ;hang up
 sta modreg
 lda #$80
 sta $a2
rddel1          ;2 second delay
 lda $a2        ;before restart
 bne rddel1
rgobak
 lda #0
 sta whahap     ;set redial flag
 jmp dalfin     ;back to phbook
;
;tone dial for 1660
tonedl
 tax
tonin
 cmp #$04
 bcc tonout
 sbc #$03
 bcs tonin
tonout
 asl a
 tay
 lda toncol-2,y
 sta $d400
 lda toncol-1,y
 sta $d401
 txa
 clc
 adc #$02
 ldy #$ff
 sec
tonrw1
 sbc #$03
 iny
 bcs tonrw1
 tya
 asl a
 tay
 lda tonrow-2,y
 sta $d407
 lda tonrow-1,y
 sta $d408
 lda #$11
 sta $d404
 sta $d40b
 lda #$fd
 sta $a2
tontml
 lda $a2
 bne tontml
 sta $d404
 sta $d40b
 lda #$fc
 sta $a2
tontm2
 lda $a2
 bne tontm2
 jmp dlgetn
;
;row and col freq's for tone dial
tonrow .byt 168,44,85,49,150,54,74,60
toncol .byt 117,77,152,85,161,94
;
caldig    ;convert ascii alpha-
 and #$7f ;numerc to t.t. value
 cmp #'1'
 bcs cald4
 cmp #'0'
 bne cald2
 ldx motype
 cpx #$03
 beq caldd
 lda #$0a
 rts
caldd
 lda #$0b
 rts
cald2
 cmp #'*'
 bne cald3
 lda #10
 rts
cald3
 cmp #'#'
 bne calder
 lda #12
 rts
calder
 and #$00
 rts
cald4
 cmp #':'
 bcs cald5
 and #$0f
 rts
cald5
 cmp #'a'
 bcc calder
 cmp #'z'
 bcs calder
 cmp #'q'
 beq calder
 bcc cald6
 sbc #$01
cald6
 and #$3f
 clc
 adc #$02
 ldy #$00
 sec
cald7
 sbc #$03
 iny
 bcs cald7
 tya
 rts
rt1660  ;1660 prompt rout.
 lda #3
 sta whahap
 jmp dalfin  ;temp bypass
rdtbeg
 lda #<stattx
 ldy #>stattx
 jsr outstr
 lda #<rdttxt
 ldy #>rdttxt
 jsr outstr
rdtlop
 jsr getin
 and #$7f
 cmp #'a'
 bne rdtl1
 ora #$80
 jsr chrout
 jmp dgobak
rdtl1
 cmp #'r'
 bne rdtl2
 ora #$80
 jsr chrout
 jmp rgobak
rdtl2
 cmp #'t'
 beq rdtl3
 cmp #$0d
 bne rdtlop
rdtl3
 jmp term
;
outmod
 jsr outstr
outmo1  lda #$e0
 sta $a2
outmo2  lda $a2
 bne outmo2
 rts
;
pnptxt .byt '%e',0
prptxt .byt '%f',0
pydtxt .byt '%d',0
atdtxt .byt 'atdt ',0
atptxt .byt 'atdp ',0
pretxt .byt 'ate0',13,0
.if v55plus
pr2txt .byt 'atm1',13,0
.else
pr2txt .byt 'atm0',13,0
.endif
pr3txt .byt 'atv1',13,0
bustxt .byt 'Busy.',0
nantxt .byt 'No carrier.',0
conntx .byt 'Connect!',0
tdelay .byt 00
smrtdl
 lda motype
 sec
 sbc #$02    ;tdelay for $a1:
 and #$07    ;03 for 1670
 asl a       ;06 for paradyne
 sta tdelay
 ldx #$00
smrtd2  lda numbuf,x
 cmp #$0d
 beq haydal
 cmp #','
 beq smrtd4
 cmp #'w'
 beq smrtd4
smrtd3  inx
 bne smrtd2
 beq haydal
smrtd4  inc tdelay
 bne smrtd3
haydal      ;hayes/paradyne dial
 txa
 lsr a ;add length of #/8
 lsr a ;to tdelay
 lsr a
 clc
 adc tdelay
 sta tdelay
 ldx tonpul
 bne haydoo
 clc       ;add 20 secs if pulse
 adc #5
 sta tdelay
haydoo
 lda motype
 cmp #$06   ;paradyne modem?
 bcc hayda0 ;nope
parda1
 ldx #$05
 jsr chkout
 lda #<prptxt
 ldy #>prptxt
 jsr outmod
 lda #' '
 jsr chrout
 lda #'4'
 jsr chrout
 lda #$0d
 jsr chrout
 jsr outmo1
 lda #<pnptxt
 ldy #>pnptxt
 jsr outmod
 lda #' '
 jsr chrout
 lda #'1'
 jsr chrout
 lda #$0d
 jsr chrout
 jsr outmo1
 lda #<pydtxt
 ldy #>pydtxt
 jsr outmod
 lda #' '
 jsr chrout
 lda tonpul
 bne parda2
 lda #'i'
 jsr chrout
parda2
 jmp haydas
hayda0
 lda baudrt
 cmp #$07    ;2400 baud?
 bne haydab
 lda #$06    ;go to 1200 to
 jsr baudst  ;send commands
haydab
 ldx #$05
 jsr chkout
 lda #<pretxt
 ldy #>pretxt
 jsr outmod
 lda #<pr2txt
 ldy #>pr2txt
 jsr outmod
 lda #<pr3txt
 ldy #>pr3txt
 jsr outmod
 jsr baud   ;back to right baud
 lda tonpul
 beq hayda2
 lda #<atdtxt
 ldy #>atdtxt
 bne hayda3
hayda2  lda #<atptxt
 ldy #>atptxt
hayda3  jsr outstr
haydas  ldx #$00
hayda4  stx numptr
 jsr clrchn
 ldx numptr
 lda #14
 sta 56223,x
 ldx #$05
 jsr chkout
 ldx numptr
 lda numbuf,x
 jsr chrout
 inx
 cmp #$0d
 bne hayda4
 jsr clrchn
 lda $029b      ;clear modinput buf
 sta $029c
 lda #$e0
 ldx motype
 cpx #$05
 beq haydap
 lda #$a0
haydap  sta $a2
 lda $a2
hayda5
 lda $a2
 bne hayda5
 lda $029b      ;clear modinput buf
 sta $029c
 lda #$00
 sta $a1
 sta $a2
hayda6  ldx #$05
 jsr chkin
 jsr getin
 and #$5f
 cmp #'u'    ;"b<u>sy"
 beq haybak
 cmp #'n'    ;"<n>o carrier"
 beq haynan
 cmp #'c'    ;"<c>onnect"
 beq haycon
 jsr clrchn
 jsr getin
 cmp #$03    ;run/stop
 bne hayda7
 ldx #$05
 jsr chkout
 lda #$0d
 jsr chrout
 jsr clrchn
 lda $029b
 lda $029c
 jmp dlabrt
hayda7
 lda $a1
 cmp tdelay
 bcc hayda6
hayda8
 lda $a2
 cmp #$90
 bcc hayda6
 ldx #$05
 jsr chkout
 lda #$0d
 jsr chrout
 jsr chrout
haynan
 jsr clrchn
 lda #<nantxt
 ldy #>nantxt
 jsr prtstt
 jmp haybk2
haybak
 jsr clrchn
 lda #<bustxt
 ldy #>bustxt
 jsr prtstt
haybk2
 lda #$c8
 sta $a2
haybk3  lda $a2
 bne haybk3
 jsr haydel
 jmp redial
haycon  jsr haydel
 lda #1     ;set connect flag
 sta whahap
 jmp dalfin
haydel
 lda #$e8
 sta $a2
 ldx #$05
 jsr chkin
haydll  jsr getin
 cmp #$0d
 beq haydlo
 lda $a2
 bne haydll
haydlo  lda $029b
 sta $029c
 jsr clrchn
 rts
;
losvco
 lda #<svctxt
 ldy #>svctxt
 ldx #16
 jsr inpset
 lda #<conffn
 ldy #>conffn
 jsr outstr
 jsr inputl
 beq losvex
 txa
 ldx #<inpbuf
 ldy #>inpbuf
 jsr setnam
 lda #2
 ldx diskdv
 ldy #0
 jsr setlfs
 ldx $b7
losvex
 rts
svconf
 jsr losvco
 bne svcon2
 rts
svcon2
 ldx #15
 jsr chkout
 ldx #0
svcon3 lda scracf,x
 beq svcon4
 jsr chrout
 inx
 bne svcon3
svcon4
 ldx #0
svcon5  lda inpbuf,x
 jsr chrout
 inx
 cpx max
 bcc svcon5
 lda #$0d
 jsr chrout
 jsr clrchn
 lda #<config
 sta nlocat
 lda #>config
 sta nlocat+1
 lda #nlocat
 ldx #<endsav
 ldy #>endsav
 jsr $ffd8
 jsr losver
losvab rts
loconf
 jsr losvco
 beq losvab
loadcf
 ldx #<config
 ldy #>config
 lda #0  ;load
 jsr $ffd5
 jsr losver
 jsr selmdm
 jsr baud
 rts
losver
 ldx #15
 jsr chkin
losve2  jsr getin
 cmp #$0d
 bne losve2
 jmp clrchn
;
selmdm
 lda motype
 bne selmd2
md1650
 lda #$00
 sta motype
mostrd     ;supposedly standard
 lda #$20  ;settings
 sta mopo1
 sta mopo3
 lda #$00
 sta mopo2
 sta mopo4
 rts
selmd2
 cmp #$01
 bne selmd3
mhesii
 lda #$01
 sta motype
 lda #$00
 sta mopo1
 lda #$26
 sta mopo2
 lda #$02
 sta mopo3
 lda #$32
 sta mopo4
 rts
selmd3
 cmp #$02
 bne selmd4
md1660
 lda #$02
 sta motype
mnewsd    ;c='s new "standard"
 lda #$00
 sta mopo1
 sta mopo3
 lda #$20
 sta mopo2
 sta mopo4
 rts
selmd4
 cmp #$03
 bne selmd5
mt1660       ;same modem, but
 lda #$03    ;with carr. detect
 sta motype
 jmp mnewsd
selmd5
 cmp #$04
 bne selmd6
mp1064       ;<-this modem sucks!
 lda #$04
 sta motype
 lda #$7e
 sta mopo1
 sta mopo3
 lda #$5e
 sta mopo2
 sta mopo4
 rts
selmd6
 cmp #$05
 bne paradm
hayesm      ;1670/hayes compat
 lda #$05
 sta motype
 jmp mnewsd
paradm
 lda #$06
 sta motype
 jmp mnewsd
;
disnmi
 pha
 txa
 pha
 tya
 pha
 lda #$7f
 sta $dd0d
 ldy $dd0d
donnmi jmp $fe72
quittm
 jsr decode
 lda #$37
 sta $01
 lda #$fe
 pha
 lda #$65
 pha
 rts          ;r/s restore
decode
.if .not(historical)
 rts
.else
 lda #0
 sta locat
 lda #>stadec
 sta locat+1
 ldy #<stadec
deco1  lda (locat),y
 eor #$aa
 sta (locat),y
 iny
 bne deco2
 inc locat+1
deco2
 cpy #<quittm
 bne deco1
 lda locat+1
 cmp #>quittm
 bcc deco1
 lda #>decoen
 sta locat+1
 ldy #<decoen
deco3  lda (locat),y
 eor #$aa
 sta (locat),y
 iny
 bne deco4
 inc locat+1
deco4
 cpy #<endall
 bne deco3
 lda locat+1
 cmp #>endall
 bcc deco3
 rts
.byt 0
decoen
.endif
viewmg
 lda #<ampag1
 ldy #>ampag1
 jsr outstr
 lda #0
 sta 198
viewm1
 lda 198
 beq viewm1
 lda #<ampag2
 ldy #>ampag2
 jsr outstr
 lda #0
 sta 198
viewm2
 lda 198
 beq viewm2
 rts
;
prwcmc
 lda macxrg
 and #$c0
 asl a
 rol a
 rol a
 asl a
 clc
 adc #'1'
 sta edfktx
 rts
edtmtx .byt $93,5,13,13,'Edit which macro?',13
.byt 158,'(','CTRL F1,F3,F5,F7, or RETURN '
.byt 'to abort.) ',5,3,2,18,0
edtrtx .byt 19,13,5,'Edit F'
edfktx .byt '1 macro...<CTRL-X> to end:',13,13,13,13,0
wchmac .byt 0
edtmac
 lda #<edtmtx
 ldy #>edtmtx
 jsr outstr
 jsr savech
edtmlp  lda 197
 cmp #1  ;return
 bne edtmc2
edtmab  rts
edtmc2 cmp #3
 bcc edtmlp
 cmp #7
 bcs edtmlp
 pha
 jsr restch
 pla
 tax
edtmc3
 lda 197
 cmp #7
 bcc edtmc3
 jsr prmacx
 sta wchmac
edtmen
 lda #0
 sta 198
 lda #$93
 jsr chrout
 lda #0
 sta $d020
 sta $d021
edtstr
 jsr prwcmc
 lda #<edtrtx
 ldy #>edtrtx
 jsr outstr
 lda #1
 sta macmdm
 sta cursfl
 lda wchmac
 sta macxrg
 jsr restch
 lda #' '
 jsr chrout
 lda #157
 jsr chrout
 jsr prtmc0
edtinp jsr curprt
edtkey
 jsr getin
 beq edtkey
 cmp #16 ;ctrl-p
 beq edtmen
 cmp #19    ;no home or clr
 beq edtkey
 cmp #$93
 bne edtky1
 ldx macxrg
edtclr
 lda #0
 sta macmem,x
 cpx wchmac
 beq edtky0
 dex
 jmp edtclr
edtky0 ldx wchmac
 stx macxrg
 jmp edtmen
edtky1
 cmp #24 ;ctrl-x
 beq edtbye
 cmp #20 ;del
 bne edtky2
 lda macxrg
 cmp wchmac
 beq edtkey
 tax
 jsr edtdel
 bcs edtmen
 lda macxrg
 and #$3f
 cmp #$3f
 bne edtkey
 jmp edtmen
edtky2
 ldx 214
 cpx #23
 bcs edtkey
 cpx #3
 bcc edtkey
edtky3
 ldx macxrg
 cpx #255
 bcs edtkey
 sta macmem,x
 pha
 txa
 cmp wchmac
 beq edtky4
 and #$3f
 bne edtky4
 pla
 jsr bell
 jmp edtmen
edtky4
 inc macxrg
 jsr curoff
 pla
 jsr ctrlck
 bcc edtky5
 jmp edtinp
edtky5
 jsr chrout
 jsr qimoff
 jmp edtinp
edtbye  ldx macxrg
 lda #0
 sta macmem,x
 rts
macrvs .byt 146
maccty .byt 10
maccol .byt 5
maccas .byt 14
macbkg .byt 0
edtdel
 lda #146
 sta macrvs
 lda #10
 sta maccty
 lda #5
 sta maccol
 lda #14
 sta maccas
 lda #0
 sta macbkg
 lda macmem-1,x
 cmp #$5F
 beq edtde2
 and #$7f
 cmp #' '
 bcc edtde0
 jmp edtdle
edtde0
 cmp #17
 beq edtde1
 cmp #29
 bne edtde3
edtde1  lda macmem-1,x
edtdeo  eor #$80
 jmp edtdln
edtde2
 lda #148
 jsr edprrv
 lda #29
 jmp edtdln
edtde3 lda macmem-1,x
 cmp #148
 bne edtde4
 lda #29
 jsr edprrv
 lda #148
 bne edtdeo
edtde4  jsr edtcok
 bmi edtde7
 ldx macxrg
 lda macmem-2,x
 sta macbkg
edtde5  dex
 cpx wchmac
 beq edtde6
 lda macmem-1,x
 jsr edtcok
 bmi edtde5
 ldy macmem-2,x
 cpy macbkg
 beq edtdcl
 cpy #2
 beq edtde5
 ldy macbkg
 cpy #2
 beq edtde5
edtdcl
 sta maccol
edtde6
 lda macbkg
 cmp #2
 bne edtclh
 sta lastch
 cpx wchmac
 beq edtclb
 lda maccol
 jsr edtcok
 bmi edtclb
 tya
 tax
edtclb
 stx $d020
 stx $d021
 jmp edtdla
edtclh
 lda #0
 sta lastch
 lda maccol
 jmp edtdln
edtde7
 cmp #10
 beq edtde8
 cmp #11
 bne edtd12
edtde8 ldx macxrg
edtde9 dex
 cpx wchmac
 beq edtd11
 lda macmem-1,x
 cmp #10
 beq edtd10
 cmp #11
 bne edtde9
edtd10  sta maccty
edtd11  lda maccty
 jmp edtdln
edtd12  and #$7f
 cmp #18
 bne edtd15
 ldx macxrg
edtd13  dex
 cpx wchmac
 beq edtd14
 lda macmem-1,x
 and #$7f
 cmp #18
 bne edtd13
 lda macmem-1,x
 sta macrvs
edtd14  lda macrvs
 and #$80
 eor #$80
 sta 199
 lda macrvs
 jmp edtdln
edtd15
 cmp #12
 beq edtd16
 cmp #14
 beq edtd16
 cmp #21
 bne edtd19
edtd16 ldx macxrg
edtdlc dex
 cpx wchmac
 beq edtd18
 lda macmem-1,x
 cmp #12
 beq edtd17
 cmp #14
 beq edtd17
 cmp #21
 bne edtdlc
edtd17 sta maccas
edtd18 lda maccas
 jmp edtdln
edtd19
 cmp #$0d
 bne edtdla
 lda #0
 sta 199
 lda #146
 jsr edprrv
 dec macxrg
 ldx macxrg
 lda #0
 sta macmem,x
 sec
 rts
edtdle
 lda #20
 jsr edprrv
 lda #148
edtdln
 jsr edprrv
edtdla
 dec macxrg
 ldx macxrg
 lda #0
 sta macmem,x
 clc
 rts
edprrv
 sta $02
 lda 199
 pha
 lda #0
 sta 199
 jsr curoff
 lda $02
 jsr ctrlck
 bcs edprr2
 jsr chrout
 jsr qimoff
edprr2 pla
 sta 199
 jmp curprt
edtcok
 ldy #15
edtco2  cmp clcode,y
 beq edtco3
 dey
 bpl edtco2
edtco3 rts
;
f7    ;terminal params/dial
 lda #0
 sta $d020
 sta $d021
 lda #<f7mtxt   ;print f7 menu
 ldy #>f7mtxt
 jsr outstr
 lda #<f7mtx2
 ldy #>f7mtx2
 jsr outstr
f7opts
 lda #$00
 sta $c6
 jsr f7parm
f7chos
 lda $a2
 and #$0f
 bne f7chgk
 lda $a2
 and #$10
 beq f7oprt
 lda #<prret
 ldy #>prret
 jsr outstr
 jmp f7chgk
f7oprt
 lda #<prret2
 ldy #>prret2
 jsr outstr
f7chgk
 jsr getin
 cmp #$00
 beq f7chos
f7chs0
 and #$7f
 cmp #'a'   ;auto-dial opt
 bne f7chs1
 lda baudrt
 sta bautmp
 lda grasfl
 sta gratmp
 jmp phbook
f7chs1
 cmp #'b'
 bne f7chs2
;baud rate change
 inc baudrt
 lda baudrt
 and #$07
 sta baudrt
 jsr baud
 jmp f7opts
f7chs2
 cmp #'d'
 bne f7chs3
;duplex change
 lda duplex
 eor #$01
 sta duplex
 jmp f7opts
f7chs3
 cmp #'m'
 bne f7chsp
;change modem type
 inc motype
 lda motype
 cmp #$07
 bcc incmod
 lda #$00
 sta motype
incmod
 jsr selmdm
 jmp f7opts
f7chsp
 cmp #'p'
 bne f7chs4
 lda protoc
 eor #1
 and #1
 sta protoc
 jmp f7opts
f7chs4
 cmp #'q'
 bne f7chs5
 jmp quittm
f7chs5
 cmp #'*'
 bne f7chs6
 lda tonpul
 eor #$01
 sta tonpul
 jmp f7opts
f7chs6
 cmp #'s'
 bne f7chs7
 jsr svconf
 jmp f7
f7chs7
 cmp #'l'
 bne f7chs8
 jsr loconf
 jmp f7
f7chs8
 cmp #'e'
 bne f7chs9
 jsr edtmac
 jmp f7
f7chs9
 cmp #'v'
 bne f7chsa
 lda visaut
 beq f7gbkk
 jsr viewmg
 jmp f7
f7chsa
 cmp #$0d
 beq f7chsb
f7gbkk jmp f7chos
f7chsb
 jmp term
;
scracf .byt 's0:',0
svctxt .byt $93,13,5,'Filename: ',0
conffn .byt 'ccgms/phone',0
f7thob .byt 2
f7mtxt .byt $93,16,14,5,'   Dialer/Parameters',13
 .byt 31,'   ',163,163,163,163,163,163,163,163,163,163,163,163,163,163
 .byt 163,163,163,13,5
f7mtx1 .byt 16
.byt 32,2,'auto-Dialer/Phone Book',13,13
.byt 32,2,'baud Rate   -',13,13
.byt 32,2,'duplex      -',13,13
.byt 32,2,'modem Type  -',13,13
.byt 32,2,'protocol    -',13,13
.byt 32,2,'edit Macros',13,13
.byt 32,2,'load/',2,'save Phone Book & Config.',13,13
visaut .byt 16
.byt 32,2,'view ',"Author's ",'Message',13,13,0
f7mtx2 .byt 32,2,'quit - Exit to BASIC',13
prret .byt 3,20,0,5,'Press <',158,18,'RETURN',146,5,'> to abort.'
 .byt 13,13,5,'Call the ',152,'Digital ',150,'P',158,'A',153,'I',159,'N'
 .byt 154,'T',32,5,'Palace...',17,157,157,157,157,157,157,157
 .byt 30,'(',158,'817',30,')',158,'281',30,'-',158,'7009',5,145,13,0
prret2 .byt 3,20,7,159,'RETURN',13,0
ttdtxt .byt 3,8,31,18,161,42,182,146,'Tone ',0
ttptxt .byt 3,8,31,18,161,42,182,146,'Pulse',0
ttntxt .byt 3,8,33,'      ',0
;
bpsspd .byt 44,1,94,1,144,1,194,1,244,1,38,2,176,4,96,9 ;rates
bdntsc .byt $94,$79,$b0,$a0,$00
.if historical
;a missing comma here after the first element was silently ignored by the cbm
;assembler, causing the bdpal symbol to be incorrectly defined using the 
;bytes which happend to follow
bdpal  .byt $94; $70,$8a,$a9,$9a
.else
bdpal  .byt $94,$70,$8a,$a9,$9a
.endif
bdrati .byt $72,$c3,$bb,$f3,$a8
bdaddc .byt $81,$02,$d0,$e5,$60
parabd .byt '%s',0
baud
 ldx motype
 cpx #$06
 bne baud0
 jsr prdspd
baud0
 lda baudrt
baudst
 asl a
 tax
 lda bpsspd,x
.if hack24 .and(.not(historical))
 ; temporary hackish fix for now in lieu of rewriting chrin/chrout routines
 cpx #14  ; if its <2400 baud, skip patch
 bcc baudst2
 sec
 sbc #$03 ; decrement 2400 baud rate a little so we can keep up.
baudst2
.endif
 sta $02
 lda bpsspd+1,x
 sta $03
 ldy $02
 lda $03
 jsr $b391  ;baud to fac1
 lda #<bdntsc
 ldy #>bdntsc
 ldx $02a6  ;ntsc/pal flg
 beq baud2
 lda #<bdpal
 ldy #>bdpal
baud2
 jsr $bb0f  ;fac1=fac1/ptr
 jsr $bbca  ;fac1 to $57
 jsr $b7f7  ;to y/a
 sty bdoutl
 sta bdouth
 ldy $02
 lda $03
 cmp #>400
 bcc baud2a
 bne baud3
 cpy #<400
 bcs baud3
baud2a
 lda #$57
 ldy #$00
 jsr $bba2
 jmp baud4
baud3
 ldy $02
 lda $03
 jsr $b391  ;baud to fac1
 lda #<bdrati
 ldy #>bdrati
 jsr $ba28  ;fac1=fac1*ptr
 lda #<bdaddc
 ldy #>bdaddc
 jsr $b867  ;fac1=fac1+ptr
 lda #$57
 ldy #$00
 jsr $ba28  ;fac1=fac1*stored
baud4
 jsr $b7f7  ;to y/a
 sty $0299    ;timing const.
 sta $029a
 ldy bdoutl
 lda bdouth
 sty $0295    ;non-std bps
 sta $0296
 lsr $0296
 ror $0295
 lda $0295
 sec
 sbc #100   ;b/2-100
 sta $0295
 lda $0296
 sbc #0
 sta $0296
 lda $029b
 sta $029c
.if toward24 .and(.not(historical))
 jsr rssetup ; use George Hug's NMI/chkin/receive  kernel patches
; but for some reason his chrout/bsout doesn't work for 2400
; baud.  Keep our own chrout vector impl:
.endif
 jmp outvec ;change chrout vec.
prdspd
 lda baudrt
 beq prdsp2
 cmp #$06
 beq prdsp2
 rts
prdsp2
 eor #$06
 jsr baudst
prdsp3
 ldx #$05
 jsr chkout
 lda #<parabd
 ldy #>parabd
 jsr outmod
 lda #' '
 jsr chrout
 lda #'1'
 ldx baudrt
 beq prdsp4
 lda #'2'
prdsp4  jsr chrout
 lda #$0d
 jsr chrout
 jmp clrchn
;
prmopt .byt <op1txt,>op1txt,<op2txt,>op2txt,<op3txt,>op3txt
prmlen .byt 4,17,6
op1txt .byt 'Full','Half'
op2txt .byt '1650 compatibles '
 .byt 'HES II/Mitey Mo  1660/modem300    '
 .byt '1660 + CD        MPP 1064         '
 .byt '1670/Hayes       Paradyne DTU     '
op3txt .byt 'Punter','XModem'
prmtab
 lda #$0d
 jsr chrout
 jsr chrout
 ldx #17
 jmp outspc
prmclc
 tya
 asl a
 tax
 lda prmopt,x
 sta prmadr+1
 lda prmopt+1,x
 sta prmadr+2
 rts
prmprt
 dex
 bmi prmpr2
 lda prmadr+1
 clc
 adc prmlen,y
 sta prmadr+1
 lda prmadr+2
 adc #$00
 sta prmadr+2
 bne prmprt
prmpr2
 inx
prmadr
 lda op1txt,x
 jsr chrout
 inx
 txa
 cmp prmlen,y
 bne prmadr
 jmp prmtab
;
f7parm
 lda #19
 jsr chrout
 lda #1
 sta 646
 ldy f7thob
prmlop
 jsr prmtab
 dey
 bne prmlop
 jsr prmclc
 lda baudrt
 asl a
 tax
 lda bpsspd+1,x
 pha
 lda bpsspd,x
 tax
 pla
 jsr outnum
 lda #' '
 jsr chrout
 jsr chrout
 jsr prmtab
 ldy #0
 jsr prmclc
 ldx duplex
 jsr prmprt
 iny
 jsr prmclc
 ldx motype
 jsr prmprt
 lda motype
 cmp #$02
 bcc notogm
 cmp #$04
 beq notogm
ystogm
 lda tonpul
 beq pltogm
 lda #<ttdtxt
 ldy #>ttdtxt
 bne prtogm
pltogm  lda #<ttptxt
 ldy #>ttptxt
prtogm  jsr outstr
 jsr prmtab
 ldy #2
 jsr prmclc
 ldx protoc
 jmp prmprt
notogm  lda #<ttntxt
 ldy #>ttntxt
 bne prtogm
;
prtvec .byt $ca,$f1
outvec  ;change chrout vec.
 lda $0326
 cmp #<printv
 bne outv1
 lda $0327
 cmp #>printv
 beq outv2
outv1 lda $0326
 sta prtvec
 lda $0327
 sta prtvec+1
 lda #<printv
 sta $0326
 lda #>printv
 sta $0327
outv2 rts
printv  ;new chrout w/corrected
 pha    ;modem timing
 lda $9a
 cmp #$03
 bne outv3
 pla
 cmp #20
 bne outndl
 lda 646
 pha
 lda #1
 sta 646
 lda #20
 jsr $e716
 pla
 sta 646
 lda #20
 clc
 rts
outndl
 jmp (prtvec)
outv3 bcc outv4
 pla
 jmp (prtvec)
outv4 lsr a
 pla
 sta $9e
 txa
 pha
 tya
 pha
 bcc outv9
 jsr $f80d
 bne outv5
 jsr $f864
 bcs outv7
 lda #$02
 ldy #$00
 sta ($b2),y
 iny
 sty $a6
outv5 lda $9e
 sta ($b2),y
outv6 clc
outv7 pla
 tay
 pla
 tax
 lda $9e
 bcc outv8
 lda #$00
outv8  rts
outv9  jsr outv11
 jmp outv6
outv10 jsr outv12
outv11 ldy $029e
 iny
 cpy $029d
 beq outv10
 sty $029e
 dey
 lda $9e
 sta ($f9),y
outv12 lda $02a1
 lsr a
 bcs outv13
 lda #$10
 sta $dd0e
 lda bdoutl
 sta $dd04
 lda bdouth
 sta $dd05
 lda #$81
 jsr $ef3b
 jsr $ef06
 lda #$11
 sta $dd0e
outv13  rts
;;; .fil 5d.gs
;dummy label for mccc expansion
;in mccc vers, .fil mccc.exp
;goes here instead
ctrlv  jmp main2
;
config
baudrt .byt $07 ;1200 baud def
tonpul .byt 0   ;0=pulse, 1=tone
mopo1  .byt $20 ;pick up
mopo2  .byt $00 ;hang up
mopo3  .byt $20 ;click on (pu)
mopo4  .byt $00 ;click off (hu)
;
motype .byt $00 ;0=1650, 1=hes ii
;^modem type^   ;2=1660/modem 300
                ;3=1660w/car.det.
                ;4=vip mpp 1064
                ;5=1670/hayes
                ;6=paradyne dtu
;
.if toward24 .and (.not(historical))
.include "newmodem2400.s"
.endif

;
phbmem ;reserve mem for phbook
.byt 0,6,'Dig. Paint Palace ','281-7009'
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
.byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
macmem
macmm1 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
macmm2 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
macmm3 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
macmm4 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
protoc .byt 0; punter/xmodem flag
endsav .byt 0
endprg .byt 0
ampag1
.byt 147,10,155,15,14,'   ',28,'C',129,'C',158,'G',30,'M',31,'S',156
.byt '! ',5,'Term '
.if historical
.byt '5.5 '
.else
.byt '2016a '
.endif
.byt 'by Craig Smith',13,13,' ',152,'(Warning! This '
.byt 'message will self-',13,'  destruct when you use the buffer!)',13,13,18
.byt 158,'Other Misc. Commands:',146,13,13,5,'SHFT STOP'
.byt '      ',159,'Disconnect. (Non-Hayes)',13,31,'C',28,'=   ',5
.byt 'STOP      ',159,'Pick up phone.  ',39,39,13,5,'SHFT ',$5f,'  '
.byt '       ',153,'Toggle cursor type',13,31,'C',28,'=   ',5,$5f,'         '
.byt 153
.byt 'Toggle ALL-CAPS mode',13,5,'SHFT CTRL'
.byt ' 1-4  ',158,'Take a ',39,'snapshot',39,' of the',13,'              '
.byt '  screen into storage 1-4',13,31,'C',28,'=   ',5,'CTRL 1-4  ',158
.byt 'Recall snapshot 1-4',13,'               (Swaps w/current screen)'
.byt 13,5,'CTRL F1-F7     ',156,'Macros.',13,13
.byt 159,'At disk prompt, "#x" changes to dev#x.',13
.byt 158,'Double-click ',31,'C',28,'= ',5,'F7 ',158,'to clear buffer',13
.byt 'before storing screen.',13
.byt 154,'At the'
.byt ' buffer cmd prompt, ',5,'< ',154,'and ',5,'>',13,154,'move the buf'
.byt 'fer pointer.',13,153,'On-line, ',5,'CTRL-B <color-code> '
.byt 153,'changes',13,'the background color.   ',5,'Press a key...',0
ampag2
.byt 147,10,155,15,14,'      ',28,'C',129,'C',158,'G',30,'M',31,'S',5,'! Term '
.if .not(historical)
.byt '2016a (C) 2016',13
.else
.byt '5.5, (C) 1988',13
.endif
.byt ' by Craig Smith, All Rights Reserved.',13,13
.if .not(historical)
.byt 153,'This program is open-source.',13
.byt 'Redistribution and use in source and',13
.byt 'binary forms, with or without modifi-',13
.byt 'cation, are permitted under the terms',13
.byt 'of the BSD 3-clause license.',13,13
.byt 'For details, or to contribute, visit:',13
.byt 158,' https://github.com/spathiwa/ccgmsterm',13,13
.byt 153
.else
.byt 153,'This program is ',39,'Share-Ware'
.byt '.',39,13,'You are granted a limited license to',13,'use, copy, &'
.byt ' distribute this program',13,'in its ',155,'UNMODIFIED ',153
.byt 'form.  If you have',13,"any suggestions or comments, if you'd"
.byt 13,'like a copy of the ML source code and',13,'permission to mod'
.byt "ify it, or if you'd",13,'just like to send a couple of bucks',13
.byt 'or whatever you think the term is',13,'worth (and greatly increase '
.byt 'the',13,'likelihood of future versions),',13,'write me at:   ',158
.byt 'Craig Smith',13,'               7437 Deaver Dr.,',13,'   '
.byt '            Ft. Worth, TX 76180',13,'               '
.byt 'Attn: CCGMS',13,159
.byt 'Source code requests please include',13
.byt 'a disk in a reusable disk mailer,',13,'sufficient postage, and a '
.byt 'short note',13,'about how you',39,'d like to change it.',13,154
.byt 'Thanks, and I hope you like the',13,'new version!!           '
.endif
.byt 5,'Press a key...',0,0
endall
.end
