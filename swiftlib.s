
;This file has been modified slightly to be compatible
;with the ca65 assembler for use by CCGMS Term.
;Other than minor asm syntax changes and commenting out of .obj
;directives,, very few modifications have been made to the 
;original swiftlib. Those modifications are guarded
;with .ifdef ccgms.
.feature labels_without_colons 

;Original file header comments follow:

;SwiftLink/Turbo-232 v0.91 device driver, by Craig Bruce, 21-Aug-2000.

;This software is Public Domain.  It is in Buddy assembler format.

;This device driver uses the SwiftLink RS-232 Serial Cartridge, available from
;Creative Micro Designs, Inc, and also supports the extensions of the Turbo232
;Serial Cartridge.  Both devices are based on the 6551 ACIA chip.  It also
;supports the "hacked" SwiftLink with a 1.8432 MHz crystal.

;The code assumes that the kernal + I/O are in context.  On the C128, call
;it from Bank 15.  On the C64, don't flip out the Kernal unless a suitable
;NMI catcher is put into the RAM under then Kernal.  For the SuperCPU, the
;interrupt handling assumes that the 65816 is in 6502-emulation mode.

useC128 = 0    ;1=C128, 0=C64

.if useC128
.ifndef ccgms
   .org $1300
.endif
   ;.obj "@0:swiftlib128"
   slHead     = $c8  ;(1) ;buffer head pointer
   slTail     = $c9  ;(1) ;buffer tail pointer
   slFreeCnt  = $ca  ;(1) ;bytes free in buffer
   slRtsOff   = $cb  ;(1) ;must be non-zero (DTR,ints active)
   slErrors   = $b4  ;(1) ;number of bytes received in error, low byte
   slWork     = $b5  ;(2) ;temp zp work area
   slNmiExit  = $ff33     ;exit address for nmi
   useC64     = 0
.else
.ifndef ccgms
   .org $c000
.endif
   ;.obj "@0:swiftlib64"
   slHead     = $f7  ;(1) ;buffer head pointer
   slTail     = $f8  ;(1) ;buffer tail pointer
   slFreeCnt  = $f9  ;(1) ;bytes free in buffer
   slRtsOff   = $fa  ;(1) ;must be non-zero (DTR,ints active)
   slErrors   = $a7  ;(1) ;number of bytes received in error, low byte
   slWork     = $b4  ;(2) ;temp zp work area
   slNmiExit  = $febc     ;exit address for nmi
   useC64     = 1
.endif

;*----------------------------------------------------------------------------*
;  user interface jump table and variables
;*----------------------------------------------------------------------------*

slJumpTable = *
   jmp slInit      ;+0  ;( .AY=iobase, .X=hackedSlFlag )
   jmp slParms     ;+3  ;( .A=params, .X=parity ) : .CS=err#.A
   jmp slShutdown  ;+6  ;()
   jmp slGetByte   ;+9  ;() : .A=byte, .X=notEmpty, .CS=err#.A
   jmp slPutByte   ;+12 ;( .A=byte ) : .CS=err#.A
   jmp slPause     ;+15 ;()
   jmp slUnpause   ;+18 ;()
   jmp slStatus    ;+21 ;() : .A=status, .X=errorCount

slDropCnt     .byte 0,0,0,0    ;(4) ;number of bytes lost from rx buffer full

;*----------------------------------------------------------------------------*
;  global variables
;*----------------------------------------------------------------------------*

slInitialized .byte $00 ;(1) ;flag indicating driver is initialized
slStopped     .byte 0   ;(1) ;flow-stopped flag
slIoAddr      .word 0    ;(2) ;base address of swiftlink registers
slIoPtr       = slWork  ;(2) ;pointer to i/o page
slTemp        .byte 0,0,0,0    ;(4) ;temporary storage
slTurbo232    .byte 0    ;(1) ;flag indicating turbo-232
slHackedFlag  .byte 0    ;(1) ;flag indicating hacked-crystal swiftlink
slCpuSpeed    .byte 0    ;(1) ;in MHz
slSendHead    .byte 0    ;(1) ;head of send buffer
slSendTail    .byte 0    ;(1) ;tail of send buffer
slSendFreeCnt .byte 0    ;(1) ;number of bytes free in send buffer
slNmiContinue .byte $4c  ;(1) ;JMP instruction for NMI save -- continue
slNmiSave     .byte 0,0  ;(2) ;normal NMI handler
slTryHard     .byte 0    ;(1) ;how hard to try to send from send buffer
slBaudCode    .byte 0    ;(1) ;current baud in effect

slRegData     = 0       ;offset of swiftlink data register
slRegStatus   = 1       ;offset of swiftlink status register
slRegCommand  = 2       ;offset of swiftlink command register
slRegControl  = 3       ;offset of swiftlink control register
slRegClock    = 7       ;offset of turbo232 external baud-rate generator

slErrNotInitialized = $01
slErrBaudTooFast    = $02
slErrBaudNotAvail   = $03
slErrNoData         = $04
slErrOverflow       = $05

;*----------------------------------------------------------------------------*
; slInit - swiftlink initialize.
;*----------------------------------------------------------------------------*

slInit = *  ;( .AY=iobase, .X=hackedSLflag )
   ;** shut down if started
   bit slInitialized
   bpl :+
   pha
   txa
   pha
   tya
   pha
   jsr slShutdown
   pla
   tay
   pla
   tax
   pla

   ;** set io page, hacked-crystal
:  sta slIoAddr+0
   sty slIoAddr+1
   sta slIoPtr+0
   sty slIoPtr+1
   txa
   beq :+
   lda #$ff
:  sta slHackedFlag

   ;** check for turbo-232
   lda #$00
   ldy #slRegControl
   sta (slIoPtr),y
   ldy #slRegClock
   ldx #$00
   lda (slIoPtr),y
   beq :+
   dex
:  stx slTurbo232

   ;** get C128/C64 cpu speed
.if useC128
   lda $d030
   and #$01
   clc
   adc #1
   sta slCpuSpeed
.else
   lda #1
   sta slCpuSpeed
.endif

   ;** check for super-cpu at 20 MHz
   bit $d0bc
   bmi :+
   bit $d0b8
   bvs :+
   lda #20
   sta slCpuSpeed
   
   ;** initialize buffers & control
:  lda #0
   sta slHead
   sta slSendHead
   sta slTail
   sta slSendTail
   sta slErrors
   sta slStopped
   lda #255
   sta slFreeCnt
   sta slSendFreeCnt

   ;** set up nmi's
   lda $318
   ldy $319
   sta slNmiSave+0
   sty slNmiSave+1
   lda #<slNmiHandler
   ldy #>slNmiHandler
   sta $318
   sty $319
   ldx slIoAddr+0
   ldy slIoAddr+1
   txa
   clc
   adc slNmiIoRef1+0
   sta slNmiIoRef1+0
   sty slNmiIoRef1+1
   txa
   clc
   adc slNmiIoRef2+0
   sta slNmiIoRef2+0
   sty slNmiIoRef2+1
   txa
   clc
   adc slNmiIoRef3+0
   sta slNmiIoRef3+0
   sty slNmiIoRef3+1
   txa
   clc
   adc slNmiIoRef4+0
   sta slNmiIoRef4+0
   sty slNmiIoRef4+1

   ;** set default to 2400-8N1, enable interrupts
   ldy #slRegData
   lda (slIoPtr),y
   ldy #slRegStatus
   lda (slIoPtr),y
   ldy #slRegControl
   lda #$18
   bit slHackedFlag
   bpl :+
   lda #$1a
:  sta (slIoPtr),y
   ldy #slRegCommand
   lda #$01
   sta slRtsOff
   ora #$08
   sta (slIoPtr),y
   lda #$06
   sta slBaudCode

   ;** return
   lda #$ff
   sta slInitialized
   clc
   rts

;*----------------------------------------------------------------------------*
; slParms - swiftlink set communication parameters.
;
; baud rates              stops     word    |   parity
; ---------------------   -----     -----   |   ---------
; $00=50     $08=9600     $00=1     $00=8   |   $00=none
; $01=110    $09=19200    $80=2     $20=7   |   $20=odd
; $02=134.5  $0a=38400              $40=6   |   $60=even
; $03=300    $0b=57600              $60=5   |   $A0=mark
; $04=600    $0c=115200                     |   $E0=space
; $05=1200   $0d=230400
; $06=2400   $0e=future
; $07=4800   $0f=future
;*----------------------------------------------------------------------------*

slParms = *  ;( .A=params, .X=parity ) : .CS=err#.A
   ;** check initialized
   bit slInitialized
   bmi :+
   lda #slErrNotInitialized
   sec
   rts

   ;** save new parity
:  sta slTemp
   lda slIoAddr+0
   ldy slIoAddr+1
   sta slIoPtr+0
   sty slIoPtr+1
   txa
   and #%11100000
   ora #%00000001
   sta slTemp+1

   ;** check cpu speed against baud rate
   lda slTemp
   and #$0f
   cmp #$0c
   bcc :++
   ldx slCpuSpeed
   cpx #1+1
   bcc :+
   cmp #$0c
   beq :++
   cpx #4
   bcs :++
:  sec
   lda #slErrBaudTooFast
   rts
   
   ;** set baud/parameters
:  lda slTemp
   and #$0f
   tax
   lda slNormBauds,x
   bit slHackedFlag
   bpl :+
   lda slHackBauds,x
:  cmp #$ff
   bne :+
   sec
   lda #slErrBaudNotAvail
   rts
:  tax
   and #$30
   beq :+
   bit slTurbo232
   bmi :+
   sec
   lda #slErrBaudNotAvail
   rts
:  lda slTemp
   and #$0f
   sta slBaudCode
   lda slTemp
   and #%11100000
   ora #%00010000
   sta slTemp
   txa
   and #$0f
   ora slTemp
   ldy #slRegControl
   sta (slIoPtr),y
   txa
   and #$30
   beq :+
   lsr
   lsr
   lsr
   lsr
   eor #$03
   ldy #slRegClock
   sta (slIoPtr),y

   ;** set new parity
:  lda slTemp+1
   sta slRtsOff
   ora #$08
   ldy #slRegCommand
   sta (slIoPtr),y
   clc
   rts

slNormBauds = *
   .byte $ff,$ff,$ff,$05,$06,$07,$08,$0a,$0c,$0e,$0f,$10,$20,$30,$ff,$ff
slHackBauds = *
   .byte $01,$03,$04,$06,$07,$08,$0a,$0c,$0e,$0f,$ff,$ff,$00,$ff,$ff,$ff
     ;in:  0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
     ;baud50 110 134   3   6  12  24  48  96  19  38  57 115 230 exp exp
     ;out masks: $0F=slBaud, val$FF=err
     ;           $30=t232ExtBaud: $00=none, $10=57.6, $20=115.2, $30=230.4

;*----------------------------------------------------------------------------*
; slShutdown - shut down swiftlink.
;*----------------------------------------------------------------------------*

slShutdown = * ;()
   ;** check initialized
   bit slInitialized
   bmi :+
   clc
   rts

   ;** stop interrupts, drop DTR
:  lda slIoAddr+0
   ldy slIoAddr+1
   sta slIoPtr+0
   sty slIoPtr+1
   lda slRtsOff
   and #%11100010
   ora #%00000010
   ldy #slRegCommand
   sta (slIoPtr),y

   ;** restore NMI vector
   lda slNmiSave+0
   ldy slNmiSave+1
   sta $318
   sty $319

   ;** flag uninitialized
   lda #$00
   sta slInitialized
   rts

;*----------------------------------------------------------------------------*
; slGetByte - get byte from receive buffer
;   arguments:
;     none
;   return values:
;     .A = byte value retrieved from buffer
;     .X = zero if no data available, otherwise non-zero.  more specifically,
;          it returns the number of bytes available in the buffer, including
;          the byte being returned
;     .C = clear for okay, set on error with the error code in .A
;*----------------------------------------------------------------------------*

slGetByte = *  ;() : .A=byte, .X=notEmpty, .CS=err#.A
   ;** check initialized
   jsr slCheckInitialized
   bcc :+
   rts

   ;** check for bytes to send
:  ldx slSendFreeCnt
   cpx #$ff
   beq :+
   lda #$00
   jsr slTryToSend

   ;** check for buffer empty
:  lda slFreeCnt
   cmp #$ff
   bne :+
   ldx #0
   txa
   clc
   rts

   ;** check for flow stopped & enough free: release flow control
:  ldx slStopped
   beq :+
   cmp #63
   bcc :+
   lda #$00
   sta slStopped
   lda slRtsOff
   ora #%00001000
   ldy #slRegCommand
   sta (slIoPtr),y

   ;** get byte from buffer
:  ldx slHead
   ldy slReceiveBuf,x
   inc slHead
   sec
   lda #$ff
   sbc slFreeCnt  ;race condition--count may be conservative
   inc slFreeCnt
   tax
   tya
   clc
   rts

;*----------------------------------------------------------------------------*
; slPutByte - put byte to swiftlink
;   arguments:
;     .A = byte value to send
;   return values:
;     .C = clear for okay, set on error with the error code in .A
;*----------------------------------------------------------------------------*

slPutByte = *  ;( .A=byte ) : .CS=overflow
   ;** check initialized
   jsr slCheckInitialized
   bcc :+
   rts

   ;** try to send
:  ldx slSendFreeCnt
   cpx #$ff
   beq :+
   pha
   lda #$00
   jsr slTryToSend
   pla

   ;** check for send-buffer overflow
:  ldx slSendFreeCnt
   bne :+
   tax
   lda #slErrOverflow
   sec
   rts

   ;** put byte into send buffer & send
:  ldx slSendTail
   sta slSendBuf,x
   inc slSendTail
   dec slSendFreeCnt
   lda #$ff
   jsr slTryToSend
   clc
   rts

slTryToSend = *  ;( .A=tryHard )  ;assumes slIoPtr is set
   ;** check for something to send
   sta slTryHard
   lda slSendFreeCnt
   cmp #$ff
   bne :+
   rts

   ;** check for flow stopped
:  lda slStopped
   beq :+
   rts

   ;** check that swiftlink is ready to send
:  ldy #slRegStatus
   lda (slIoPtr),y
   and #$10
   bne :+
   lda slTryHard      ;keep trying if must try hard
   bmi slTryToSend
   rts

   ;** send byte and try again
:  ldx slSendHead
   lda slSendBuf,x
   ldy #slRegData
   sta (slIoPtr),y
   inc slSendHead
   inc slSendFreeCnt
   jmp slTryToSend

;*----------------------------------------------------------------------------*
; slPause - assert flow control & disable swiftlink interrupts
;   arguments:
;     none
;   return values:
;     .C = clear for okay, set on error with the error code in .A
;*----------------------------------------------------------------------------*

slPause = *  ;()
   ;** check initialized
   jsr slCheckInitialized
   bcc :+
   rts

   ;** assert flow control
:  lda slRtsOff
   sta slStopped
   ldy #slRegCommand
   sta (slIoPtr),y

   ;** delay for flow stop to be received
   ldx slBaudCode
   lda slPauseTimes,x
   jsr slDelayMs

   ;** stop rx interrupts
   lda slRtsOff
   ora #$02
   ldy #slRegCommand
   sta (slIoPtr),y
   clc
   rts

   ;** delay times: 32 byte-receive times in milliseconds, or 100 max
   ;** formula = 320,000 / baud
   slPauseTimes = *
   .byte 100,100,100,100,100,100,100,067,034,017,009,006,003,002,001,001
     ;in:  0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
     ;baud50 110 134   3   6  12  24  48  96  19  38  57 115 230 exp exp

;*----------------------------------------------------------------------------*
; slUnpause - re-enable swiftlink interrupts & release flow control
;   arguments:
;     none
;   return values:
;     none
;*----------------------------------------------------------------------------*

slUnpause = *  ;()
   ;** check initialized
   jsr slCheckInitialized
   bcc :+
   rts

   ;** re-enable rx interrupts & release flow control
:  lda #$00
   sta slStopped
   lda slRtsOff
   ora #%00001000
   ldy #slRegCommand
   sta (slIoPtr),y

   ;** poll for stalled char & exit
   jsr slPollReceive
   clc
   rts

;*----------------------------------------------------------------------------*
; slStatus - get status of swiftlink driver
;   arguments:
;   return values:
;*----------------------------------------------------------------------------*

slStatus = *  ;( ) : .A=status, .X=errorCounter, .CS=err#.A
   ;** check initialized
   jsr slCheckInitialized
   bcc :+
   rts

   ;** get status
:  ldy #slRegStatus
   lda (slIoPtr),y
   pha
   jsr slPollReceive  ;bug-recovery hack
   pla
   ldx slErrors
   clc
   rts

;*----------------------------------------------------------------------------*
; NMI handler
; C128 NMI overhead=76 cycles: int=7, maxLatency=6, ROMenter=33, ROMexit=30
; C64  NMI overhead=76 cycles: int=7, maxLatency=6, ROMenter=34, ROMexit=29
;
; timing: normal=76+43+9=128 cycles, assertFlow=76+52+9=137 cycles
;
; C128 @ 115.2k: 177 cycles avail (fast)
; C64  @  57.6k: 177 cycles avail, worstAvail=177-43? = 134
; SCPU @ 230.4k: 868 cycles avail: for a joke!
;*----------------------------------------------------------------------------*

slNmiHandler = *
.if useC64
   pha
.endif
   slNmiIoRef1 = *+1
   lda $de00+1        ;(4) ;status ;check for byte received
   and #$08           ;(2)
   beq slNmiNorm      ;(2*)
.if useC64
   cld
   txa
   pha
   tya
   pha
.endif
   slNmiIoRef2 = *+1
   lda $de00+1        ;(4) opt ;status ;check for receive errors
   and #$07           ;(2) opt
   beq :+              ;(3*)opt
   inc slErrors       ;(5^)opt
   slNmiIoRef3 = *+1
:  lda $de00+0        ;(4) ;data  ;get byte and put into receive buffer
   ldy slTail         ;(3)
   ldx slFreeCnt      ;(3)
   beq slDrop         ;(2*)
   sta slReceiveBuf,y ;(5)
   inc slTail         ;(5)
   dec slFreeCnt      ;(5)
   cpx #33            ;(2)  ;check for buffer space low
   bcc :+              ;(2*)
   jmp slNmiExit      ;(3)
:  lda slRtsOff       ;(3)  ;assert flow control if buffer space too low
   slNmiIoRef4 = *+1
   sta $de00+2        ;(4) ;command
   sta slStopped      ;(3)
   jmp slNmiExit      ;(3)

slDrop = *
   inc slDropCnt+0    ;not time-critical
   bne :+
   inc slDropCnt+1
   bne :+
   inc slDropCnt+2
   bne :+
   inc slDropCnt+3
:  jmp slNmiExit

slNmiNorm = *
.if useC64
   pla
.endif
   jmp slNmiContinue

;*----------------------------------------------------------------------------*
; slCheckInitialized  -  internal check if initialized & set slIoPtr
;   doesn't change any registers
;*----------------------------------------------------------------------------*

slCheckInitialized = *
   ;** check initialized
   bit slInitialized
   bmi :+
   lda #slErrNotInitialized
   sec
   rts

   ;** set ioptr
:  pha
   lda slIoAddr+0
   sta slIoPtr+0
   lda slIoAddr+1
   sta slIoPtr+1
   pla
   clc
   rts

;*----------------------------------------------------------------------------*
; slPollReceive - poll for rx char
;   assumes that slIoPtr is set
;   This function is useful in odd cases where the 6551 has a character in
;   it but it fails to raise an NMI.  It might be edge-triggering conditions?
;   Actually, I'm not entirely sure that this condition can still arise, but
;   calling this function does no harm.
;*----------------------------------------------------------------------------*

slPollReceive = *
   ldy #slRegStatus
   lda #$08
   and (slIoPtr),y
   beq :+
   and (slIoPtr),y
   beq :+
   ldy #slRegData
   lda (slIoPtr),y
   ldx slFreeCnt
   beq :+
   ldx slTail
   sta slReceiveBuf,x
   inc slTail
   dec slFreeCnt
:  rts

;*----------------------------------------------------------------------------*
;  slDelayMs : delay for given number of milliseconds
;    This implementation isn't very rigerous; it merely delays for the
;    approximate number of clock cycles for the processor speed.
;    Algorithm:
;       repeat for number of milliseconds: 
;          repeat for number of MHz of cpu speed:
;             delay for 1017 clock cycles
;*----------------------------------------------------------------------------*

slDelayMs = *  ;( .A=milliseconds )
:  ldy slCpuSpeed
:  ldx #203   ;(2)
:  dex        ;(2)
   bne :-      ;(3) // 1017 cycles
   dey
   bne :--
   sec
   sbc #1
   bne :---
   rts

;*----------------------------------------------------------------------------*
;  send and receive buffers: 256 bytes each
;*----------------------------------------------------------------------------*

.ifdef ccgms
slEnd = * ; ccgms instead will define the send and receive buf to $ce00/$cf00
; in the same place it used for the kernal rs232 buffers
.else
slReceiveBuf = *
slSendBuf = slReceiveBuf+256
slEnd = slSendBuf+256
.endif
