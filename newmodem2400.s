; "newmodem.src"
;
; From "Toward 2400: RS-232 revitied"
; by George Hug
; Transactor, Vol 9, Issue 3
;

; define ccgms to disable features not needed by ccgms term
; (defined when included by ccgmsterm.s)

ribuf   = $f7
robuf   = $f9
baudof  = $0299
ridbe   = $029b
ridbs   = $029c
rodbs   = $029d
rodbe   = $029e
enabl   = $02a1
rstkey  = $fe56
norest  = $fe72
return  = $febc
oldout  = $f1ca
oldchk  = $f21b
ochrin  = $f157
ogetin  = $f13e
findfn  = $f30f
devnum  = $f31f
nofile  = $f701

.ifndef ccgms
* = $ce00 
xx00 jmp rssetup
xx03 jmp inable
xx06 jmp disabl
xx09 jmp rsget
xx0c jmp rsout
     nop
.endif

; start-bit times
strt24 .word   459     ; 2400
strt12 .word   1090    ; 1200
strt03 .word   4915    ;  300
; full-bit times
full24 .word   421     ; 2400
full12 .word   845     ; 1200
full03 .word   3410    ;  300
;-----------------------------------
rssetup lda     #<nmi64
        ldy     #>nmi64
        sta     $0318
        sty     $0319
        lda     #<nchkin
        ldy     #>nchkin
        sta     $031e
        sty     $031f
        lda     #<nbsout
        ldy     #>nbsout
        sta     $0326
        sty     $0327
        lda     #<nchrin
        ldy     #>nchrin
        sta     $0324
        sty     $0325
        lda     #<ngetin
        ldy     #>ngetin
        sta     $032a
        sty     $032b
        rts
;-------------------------
nmi64   pha	; new nmi handler
        txa
        pha
        tya
        pha
nmi128  cld
        ldx     $dd07   ; sample timer B high byte
        lda     #$7f    ; disable CIA NMI's
        sta     $dd0d
        lda     $dd0d   ; read/clear flags
        bpl     notcia  ; (restore key)
        cpx     $dd07   ; timer B timeout since ldx $dd07 above?
        ldy     $dd01   ; (sample pin C)
        bcs     mask    ; no
        ora     #$02    ; tes, set flag in acc.
        ora     $dd0d   ; read/clear flags again
mask    and     enabl   ; mask out non-enabled
        tax             ; these must be serviced
        lsr             ; timer A? (bit 0)
        bcc     ckflag  ; no
        lda     $dd00   ; yes, put bit on pin M
        and     #$fb
        ora     $b5
        sta     $dd00
ckflag  txa
        and     #$10    ; *flag NMI? (bit 4)
        beq     nmion   ; no
strtlo  lda     #$42    ; yes, start-bit to timer B
        sta     $dd06
strthi  lda     #$04    ;
        sta     $dd07
        lda     #$11    ; start timer B counting
        sta     $dd0f
        lda     #$12    ; (flag NMI off, timer B on
        eor     enabl   ; update mask
        sta     enabl
        sta     $dd0d   ; enable new config.
fulllo  lda     #$4d    ; change reload latch
        sta     $dd06   ;  to full-bit time
fullhi  lda     #$03    ;
        sta     $dd07
        lda     #$08    ; # of bits to receive
        sta     $a8
        bne     chktxd  ; branch always
notcia  ldy     #$00
        jmp     rstkey  ; or jmp norest
nmion   lda     enabl   ; re-enable NMI's
        sta     $dd0d
        txa
        and     #$02    ; timer B? (bit 1)
        beq     chktxd  ; no
        tya             ; yes, sample from (sample pin C) above
        lsr
        ror     $aa     ; rs232 is LSB first
        dec     $a8     ; byte finished?
        bne     txd     ; no
        ldy     ridbe   ; yes, byte to buffer
        lda     $aa
        sta     (ribuf),y ; (no over-run test)
        inc     ridbe
        lda     #$00    ; stop timer B
        sta     $dd0f
        lda     #$12    ; timer B NMI off, *flag on
switch  ldy     #$7f    ; disable NMI's
        sty     $dd0d
        sty     $dd0d   ; twice
        eor     enabl   ; update mask
        sta     enabl
        sta     $dd0d   ; enable new config.
txd     txa
        lsr             ; timer A?
chktxd  bcc     exit    ; no
        dec     $b4     ; yes, byte finished?
        bmi     char    ; yes
        lda     #$04    ; no, prep next bit
        ror     $b6     ; (fill with stop bits)
        bcs     store
low     lda     #$00
store   sta     $b5
exit    jmp     return  ; restore regs, rti
char    ldy     rodbs
        cpy     rodbe   ; buffer empty?
        beq     txoff   ; yes
getbuf  lda     (robuf),y ; no, prep next byte
        inc     rodbs
        sta     $b6
        lda     #$09    ; # bits to send
        sta     $b4
        bne     low     ; always - do start bit
txoff   ldx     #$00    ; stop timer A
        stx     $dd0e
        lda     #$01    ; disable timer A NMI
        bne     switch  ; always
;-----------------------------------
disabl  pha             ; turns off modem port
test    lda     enabl
        and     #$03    ; any current activity?
        bne     test    ; yes, test again
        lda     #$10    ; no, disable *flag NMI
        sta     $dd0d
        lda     #$02
        and     enabl   ; currently receiving?
        bne     test    ; yes, start over
        sta     enabl   ; all off, update mask
        pla
        rts
;-----------------------------------
nbsout  pha             ; new bsout
        lda     $9a
        cmp     #$02
        bne     notmod
        pla
rsout   sta     $9e     ; output to modem
        sty     $97
point   ldy     rodbe
        lda     $9e
        sta     (robuf),y ; not official 'til sty rodbe below
        iny
        cpy     rodbs   ; buffer full?
        beq     fulbuf  ; yes
        sty     rodbe   ; no, bump pointer
strtup  lda     enabl
        and     #$01    ; transmitting now?
        bne     ret3    ; yes
        sta     $b5     ; no, prep start bit
        lda     #$09
        sta     $b4     ;  # bits to send
        ldy     rodbs
        lda     (robuf),y
        sta     $b6     ;  and next byte
        inc     rodbs
        lda     baudof  ; full TX bit time to timer A
        sta     $dd04
        lda     baudof+1
        sta     $dd05
        lda     #$11    ; start timer A
        sta     $dd0e
        lda     #$81    ; enable timer A NMI
change  sta     $dd0d   ; NMI clears flag if set
        php             ; save IRQ status.
        sei             ; disable IRQ's
        ldy     #$7f    ; disable NMI's
        sty     $dd0d
        sty     $dd0d   ; twice
        ora     enabl   ; update mask
        sta     enabl
        sta     $dd0d   ; enable new config.
        plp             ; restore IRQ status
ret3    clc
        ldy     $97
        lda     $9e
        rts
fulbuf  jsr     strtup
        jmp     point
notmod  pla             ; back to old bsout.
        jmp     oldout
;-----------------------------------
nchkin
.ifdef ccgms ; if using swiftlib, allow chkin of modem when swiftlink active
.if swiftlib ; to 'work'. Overridden chrin/getin/chrout routines check this
; to map to slGetByte/slPutByte calls
        cpx     #modemln
        bne     nchkin2
        lda     swiftout
        beq     nchkin2
        lda     #$02
        sta     $99
        rts
nchkin2
.endif
.endif
        jsr     findfn  ; new chkin
        bne     nosuch
        jsr     devnum
        lda     $ba
        cmp     #$02
        bne     back
        sta     $99
inable  sta     $9e     ; enable rs232 input
        sty     $97
bauds   lda     baudof+1 ; set receive to same
        and     #$06     ;  baud rate as xmit
        tay
        lda     strt24,y
        sta     strtlo+1 ; overwrite code setting timer B (self mod)
        lda     strt24+1,y
        sta     strthi+1
        lda     full24,y
        sta     fulllo+1
        lda     full24+1,y
        sta     fullhi+1
        lda     enabl
        and     #$12    ; flag or timer B on?
        bne     ret1    ; yes
        sta     $dd0f   ; no, stop timer B
        lda     #$90    ; turn on flag NMI
        jmp     change
nosuch  jmp     nofile
back    lda     $ba
        jmp     oldchk
;-----------------------------------
nchrin  lda     $99     ; new chrin
        cmp     #$02
        beq     rsget
        jmp     ochrin
ngetin  ldx     $99     ; new getin
        lda     #$00
        cpx     #$02
        beq     rsget
        jmp     ogetin
;
rsget   sta     $9e     ; input from modem
        sty     $97
.ifdef ccgms ; see comment at nchkin
.if swiftlib
        ldy     swiftout
        beq     rsget1
        jsr     slGetByte
        ldy     $97
        rts
rsget1
.endif
.endif
        ldy     ridbs
        cpy     ridbe   ; buffer empty?
        beq     ret2    ; yes.
        lda     (ribuf),y ; no, fetch character
        sta     $9e
        inc     ridbs
ret1    clc             ; cc = char in acc.
ret2    ldy     $97
        lda     $9e
last    rts             ; vc = buffer was empty

