; FizzBuzz for the 2600, heavily modified from:
;   How to Draw A Playfield.
;   by Nick Bensema  9:23PM  3/2/97
; Compile with dasm fizzbuzz.asm -f3 -ofizzbuzz.bin

	processor 6502
	include vcs.h
	include macro.h
	org $F000
       
FrameCount = $80
Div3 = $81
Div5 = $82
Temp = $83
HundredsDigit = $84
TensDigit = $85
OnesDigit = $86

	MAC SILENT
	lda #$0
	sta AUDV0
	sta AUDV1
	ENDM

	MAC SOUND 		; voice, freq, gen, volume
	lda {2}
	sta AUDF{1}
	lda {3}
	sta AUDC{1}
	lda {4}
	sta AUDV{1}
	ENDM

	MAC SHLEFT
	REPEAT {1}
	asl
	REPEND
	ENDM

	MAC SHRIGHT
	REPEAT {1}
	lsr
	REPEND
	ENDM
	
Start
	CLEAN_START
				;
	JSR  GameInit

MainLoop
	JSR  VerticalBlank ;Execute the vertical blank.
	JSR  GameCalc      ;Do calculations during Vblank
	JSR  DrawScreen    ;Draw the screen
	JSR  OverScan      ;Do more calculations during overscan
	JMP  MainLoop      ;Continue forever.

VerticalBlank SUBROUTINE
	LDX  #0
	LDA  #2
	STA  WSYNC  
	STA  WSYNC
	STA  WSYNC
	STA  VSYNC ;Begin vertical sync.
	STA  WSYNC ; First line of VSYNC
	STA  WSYNC ; Second line of VSYNC.
;
; But before we finish off the third line of VSYNC, why don't we
; use this time to set the timer?  This will save us a few cycles
; which would be more useful in the overscan area.
;
; To insure that we begin to draw the screen at the proper time,
; we must set the timer to go off just slightly before the end of
; the vertical blank space, so that we can WSYNC up to the ACTUAL
; end of the vertical blank space.  Of course, the scanline we're
; going to omit is the same scanline we were about to waste VSYNCing,
; so it all evens out.
;
; Atari says we have to have 37 scanlines of VBLANK time.  Since
; each scanline uses 76 cycles, that makes 37*76=2888 cycles.
; We must also subtract the five cycles it will take to set the
; timer, and the three cycles it will take to STA WSYNC to the next
; line.  Plus the checking loop is only accurate to six cycles, making
; a total of fourteen cycles we have to waste.  2888-14=2876.
;
; We almost always use TIM64T for this, since the math just won't
; work out with the other intervals.  2880/64=44.something.  It
; doesn't matter what that something is, we have to round DOWN.
;
	LDA  #44
	STA  TIM64T
;
; And now's as good a time as any to clear the collision latches.
;
	LDA #0
	STA CXCLR
;
; Now we can end the VSYNC period.
;
	STA  WSYNC ; Third line of VSYNC.
	STA  VSYNC ; (0)
;
; At this point in time the screen is scanning normally, but
; the TIA's output is suppressed.  It will begin again once
; 0 is written back into VBLANK.
;
	RTS  

; Minimal game calculations, just to get the ball rolling.
;
GameCalc SUBROUTINE
	LDA FrameCount
	ADC #4
	STA FrameCount
	;; every 64 frames, count
	beq .IncCount
	rts

.IncCount
	jsr DecCounts
	jsr ThreeCount
	jsr FiveCount
	jsr ChangeSounds
	rts

ThreeCount SUBROUTINE
	inc Div3
	lda #3
	cmp Div3
	beq .reset
	rts
.reset	
	lda #0
	sta Div3
	rts

FiveCount SUBROUTINE
	inc Div5
	lda #5
	cmp Div5
	beq .reset
	rts
.reset
	lda #0
	sta Div5
	rts


DecCounts SUBROUTINE
	lda OnesDigit
	cmp #9
	beq .inctens
	inc OnesDigit
	rts

.inctens
	lda #0
	sta OnesDigit
	lda TensDigit
	cmp #9
	beq .inchundreds
	inc TensDigit
	rts
.inchundreds
	lda #0
	sta TensDigit
	lda HundredsDigit
	cmp #1
	beq .zerodigits
	inc HundredsDigit
	rts

.zerodigits
	lda #0
	sta HundredsDigit
	rts

ChangeSounds SUBROUTINE
	SILENT
	lda Div3
	cmp #0
	bne .1
	SOUND 0,#$1f,#$08,#$03
.1
	lda Div5
	cmp #0
	bne .2
	SOUND 1,#$10,#$06,#$05
.2
	rts
	

DrawScreen SUBROUTINE
	LDA #0
	STA COLUBK 		; black background
	lda #$0f 
	sta COLUP0		; white foreground
.waitforit
	LDA INTIM
	BNE .waitforit
	STA WSYNC
	STA VBLANK  ;End the VBLANK period with a zero.

; playfield	
	LDA  #2 		; SCORE mode, seperate colors for each side
	STA  CTRLPF

	ldy #191 		; scanline countdown

.scanloop

	STA WSYNC

	;; only print the 'score' at the top
	tya
	cmp #$A0
	bmi .noprint
	cmp #$BF
	bpl .noprint

	jsr PrintScore
	dey
	bne .scanloop
	
.noprint
	lda #0
	sta PF0
	sta PF1
	sta PF2

	DEY
	BNE .scanloop

; Clear all registers here to prevent any possible bleeding.
	LDA #2
	STA WSYNC  ;Finish this scanline.
	STA VBLANK ; Make TIA output invisible,
	; Now we need to worry about it bleeding when we turn
	; the TIA output back on.
	; Y is still zero.
	STY PF0
	STY PF1
	STY PF1
	STY GRP0
	STY GRP1
	STY ENAM0
	STY ENAM1
	STY ENABL
	RTS


PrintScore SUBROUTINE
	tya
	;;  we want to load PF2 with the contents of Digit[12] + Count*8 + (8 - (Y>>2) % 7)
	SHRIGHT 2
	and #7
	sta Temp
	lda #8
	clc
	sbc Temp
	sta Temp
	
	lda OnesDigit
	and #$f
	SHLEFT 3
	adc Temp
	tax
	lda Digits2,X
	sta PF2


	lda TensDigit
	SHLEFT 3
	adc Temp
	tax
	lda Digits1,X
	sta PF1

	lda HundredsDigit
	cmp #0
	beq .zerohundred
	SHLEFT 3
	adc Temp
	tax
	lda Digits2,X 		; tricky data reuse
	SHLEFT 2
	sta PF0
	rts
	
.zerohundred
	lda #0
	sta PF0
	rts
	

OverScan SUBROUTINE   ;We've got 30 scanlines to kill.
	LDX #30
.killlines
	STA WSYNC
	DEX
	BNE .killlines
	RTS


; GameInit could conceivably be called when the Select key is pressed,
; or some other event.
GameInit SUBROUTINE
	LDA #0
	STA FrameCount
	STA OnesDigit
	STA TensDigit
	STA HundredsDigit
	RTS

	
; Badly drawn number font.  Digits1 is for PF1, Digits2 is for PF2
; (And you can take it's '1' and ASL twice for PF0)
	org $ff00
Digits1
	;; 0
	.byte %00111100
	.byte %01100110
	.byte %01000010
	.byte %01000010
	.byte %01000010
	.byte %01100110
 	.byte %00111100
	.byte %00000000
	;; 1
	.byte %00110000
	.byte %01110000
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %01111100
	.byte %00000000
	;; 2
	.byte %00111000
	.byte %00000100
	.byte %00001100
	.byte %00010000
	.byte %00100000
	.byte %01000000
	.byte %01111110
	.byte %00000000
	;; 3
	.byte %00111000
	.byte %01000100
	.byte %00000100
	.byte %00011000
	.byte %00000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 4
	.byte %00001000
	.byte %00011000
	.byte %00101000
	.byte %01001000
	.byte %11111110
	.byte %00001000
	.byte %00001000
	.byte %00000000
	;; 5
	.byte %01111000
	.byte %01000000
	.byte %01000000
	.byte %00111000
	.byte %00000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 6
	.byte %00111000
	.byte %01000000
	.byte %01000000
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 7
	.byte %11111110
	.byte %00000100
	.byte %00001000
	.byte %00010000
	.byte %00100000
	.byte %01000000
	.byte %10000000
	.byte %00000000
	;; 8
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 9
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %00000100
	.byte %00000100
	.byte %00111000
	.byte %00000000

Digits2
	;; 0
	.byte %00111100
	.byte %01100110
	.byte %01000010
	.byte %01000010
	.byte %01000010
	.byte %01100110
 	.byte %00111100
	.byte %00000000
	;; 1
	.byte %00011000
	.byte %00011100
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %00010000
	.byte %01111100
	.byte %00000000
	;; 2
	.byte %00111000
	.byte %01000100
	.byte %01100000
	.byte %00010000
	.byte %00001000
	.byte %00000100
	.byte %01111110
	.byte %00000000
	;; 3
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %01110000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 4
	.byte %00100000
	.byte %00110000
	.byte %00101000
	.byte %00100100
	.byte %11111110
	.byte %00100000
	.byte %00100000
	.byte %00000000
	;; 5
	.byte %01111100
	.byte %00000100
	.byte %00000100
	.byte %00111000
	.byte %01000000
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 6
	.byte %00111000
	.byte %00000100
	.byte %00000100
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 7
	.byte %11111110
	.byte %01000000
	.byte %00100000
	.byte %00010000
	.byte %00001000
	.byte %00000100
	.byte %00000010
	.byte %00000000
	;; 8
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %00000000
	;; 9
	.byte %00111000
	.byte %01000100
	.byte %01000100
	.byte %00111000
	.byte %01000000
	.byte %01000000
	.byte %00111000
	.byte %00000000

	org $FFFC
	.word Start
	.word Start
