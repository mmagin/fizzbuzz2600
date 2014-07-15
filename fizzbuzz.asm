; FizzBuzz for the 2600, heavily modified from:

; How to Draw A Playfield.
; by Nick Bensema  9:23PM  3/2/97


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

Start
	CLEAN_START
				;
; The above routine does not clear location 0, which is VSYNC.  We will
; take care of that later.
;
; At this point in the code we would set up things like the data
; direction registers for the joysticks and such.  
;
	JSR  GameInit
;
; Here is a representation of our program flow.
;
MainLoop
	JSR  VerticalBlank ;Execute the vertical blank.
	JSR  CheckSwitches ;Check console switches.
	JSR  GameCalc      ;Do calculations during Vblank
	JSR  DrawScreen    ;Draw the screen
	JSR  OverScan      ;Do more calculations during overscan
	JMP  MainLoop      ;Continue forever.
;
; It is important to maintain a stable screen, and this routine
; does some important and mysterious things.  Actually, the only
; mysterious part is VSYNC.  All VBLANK does is blank the TIA's
; output so that no graphics are drawn; otherwise the screen
; scans normally.  It is VSYNC which tells the TV to pack its
; bags and move to the other corner of the screen.
;
; Fortunately, my program sets VBLANK at the beginning of the
; overscan period, which usually precedes this subroutine, so
; it is not changed here.
;
VerticalBlank
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
;
; Checking the game switches is relatively simple.  Theoretically,
; some of it could be slipped between WSYNCs in the VBlank code.
; But we're going for clarity here.
;
; It just so happens that I'm not going to check any game switches
; here.  I'm just going to set up the colors, without even checking
; the B&W switch!  HA!
;
CheckSwitches
	LDA #0
	STA COLUBK  ; Background will be black.
	lda #$0f
	sta COLUP0
	RTS
;
; Minimal game calculations, just to get the ball rolling.
;
GameCalc
	LDA FrameCount
	ADC #4
	STA FrameCount
	;; every 64 frames, count
	beq IncCount
	rts

IncCount
	jsr BumpCounts
	
	inc Div3
	lda #3
	cmp Div3
	bne Not3
	lda #0
	sta Div3
	jsr Silent
	jsr Fizz
Not3

	inc Div5
	lda #5
	cmp Div5
	bne Not5
	lda #0
	sta Div5
	jsr Silent
	jsr Buzz
Not5

	lda #0
	cmp Div5
	beq GCDone
	cmp Div3
	beq GCDone
	jsr Silent
GCDone
	rts
	
	;; all of those rts themselves

BumpCounts SUBROUTINE
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
;
; This is the scariest thing I've done all month.
;
DrawScreen
	LDA INTIM
	BNE DrawScreen ; Whew!
	STA WSYNC
	STA VBLANK  ;End the VBLANK period with a zero.

; playfield	
	LDA  #2 		; SCORE mode, seperate colors for each side
	STA  CTRLPF

; I'm going to use the Y register to count scanlines this time.
; Realize that I am forfeiting the use of the Y register for this
; purpose, but DEC Zero Page takes five cycles as opposed to DEY's
; two, and LDA Zero Page takes three cycles as opposed to TYA's two.
;
; I'm using all 191 remaining scanlines after the WSYNC.  If you
; want less, or more, change the LDY line.
;
; This is a decremental loop, that is, Y starts at 191 and continues
; down to 0, as do all functions of Y such as memory locations, which
; is why the graphics at the end of this file are stored "bottom-up".
; In a way, one could say that's how the screen is drawn.  To turn this
; into an incremental loop, change the number to 255-191 (65) and change
; the DEY at the end ot the scanning loop to INY.
;
	ldy #191
;
; Okay, now THIS is scary.  I decided to put the bulk of my comments
; BEFORE the code, rather than inside it, so that you can look at the
; code all at once.
;
; Notice the new method of cycle counting I use.  I'll send an update
; to cyccount.txt RSN.
;
; This routine came out surprisingly clean.  There are no branches,
; and most updates are made even before PF0 becomes visible at cycle 23,
; even though PF1 and PF2 don't become visible until, by my estimate,
; cycles 29 and 40, respectively.  We could use this time to get player
; shape and colors from temp variables and sneak them in, but that's
; another file.  In fact, at the last minute I re-arranged things
; and threw in some color changes.
;
; The playfield will only be moved up every 4 scanlines, so it doesn't look 
; squished.  I could have updated it every 2 scanlines, and that would have 
; saved two cycles. I could have saved another two cycles by having it 
; change EVERY scanline.  Comment out one or both of the ASL's to see what 
; this would look like.  I realize that it updates the PF registers whether
; it needs it or not, but it would be pointless to branch around these
; updates.  Better to know you're wasting cycles and have them counted
; than to get unlucky and have your code spill into the next scanline
; every time too many things get updated.
;
; This is going to be a moving playfield.  For a stationary playfield,
; comment out the SEC and SBC lines.  That's probably what most of you all
; are going to want, anyway.  And for a really good moving playfield, 
; like in River Raid or Vanguard, you'll need slightly more interesting 
; code than I'm prepared to provide.
;
; I also could have made the playfield graphic 16 bytes high, or 32, or 64, 
; by changing only my data and the AND line.  AND can serve as a modulus
; for any power of two (2^n) up to 128, by ANDing a byte with that number
; minus one ( (2^n)-1 ).  8 bytes * every 4 scanlines == 32, which is
; a power of two, which is why this works.  Try commenting out the AND line
; and see how the program interprets it.  Remember that PlayfieldY goes
; up to 255.
;
; But you won't need to worry about that if you're drawing a stationary 
; playfield where the graphics data is so large, it doesn't need to repeat.
; In that case, you don't need the AND line and you don't need to make sure 
; your graphics are 2^n bytes tall.  Comment out the AND, SEC and SBC lines,
; and add a third LSR to the block of two.  It indexes a bit too far at the 
; top of the screen, which explains the garbage.  You can fix that problem
; either by adding more data to the end of each array, or by decreasing
; the resolution by adding a fourth or fifth LSR.
;
; And who's to say you'll need all three playfield registers?  Perhaps
; you have a rather narrow playfield, or one that's always clear in the
; middle.  Either choice will save you five cycles per scanline.
;
; As you can see, it can be trimmed down quite a bit, and I still have
; a lot of cycles left over.  The maximum, if you recall, is 73 if you
; plan to use STA WSYNC, and I pity the fool who doesn't.


ScanLoop

	STA WSYNC

	tya
	cmp #$60
	bmi NoPrint
	cmp #$80
	bpl NoPrint

	
	;;  we want to load PF2 with the contents of Digit[12] + Count*8 + (8 - (Y>>2) % 7)

	lsr
	lsr
	and #7
	sta Temp
	lda #8
	clc
	sbc Temp
	sta Temp
	

	lda OnesDigit
	;;  display ones
	and #$f
	asl
	asl
	asl

	adc Temp
	
	tax

	lda Digits2,X
	sta PF2

	;; display tens
	lda TensDigit
	asl
	asl
	asl

	adc Temp
	
	tax

	lda Digits1,X
	sta PF1

	;; display hundreds
	lda HundredsDigit
	cmp #0
	beq .zerohundred
	;; get the '1'
	asl
	asl
	asl
	adc Temp
	tax
	lda Digits2,X
	asl
	asl
	sta PF0
	dey
	BNE ScanLoop
	
.zerohundred
	lda #0
	sta PF0
	
	DEY
	BNE ScanLoop

NoPrint
	lda #0
	sta PF0
	sta PF1
	sta PF2

	DEY
	BNE ScanLoop

;
; Clear all registers here to prevent any possible bleeding.
;
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

;
; For the Overscan routine, one might take the time to process such
; things as collisions.  I, however, would rather waste a bunch of
; scanlines, since I haven't drawn any players yet.
;
OverScan   ;We've got 30 scanlines to kill.
	LDX #30
KillLines
	 STA WSYNC
	 DEX
	 BNE KillLines
	RTS

;
; GameInit could conceivably be called when the Select key is pressed,
; or some other event.
;
GameInit
	LDA #0
	STA FrameCount
	STA OnesDigit
	STA TensDigit
	STA HundredsDigit
	RTS


; Routines to set our fizz or buzz sound
	
Fizz
	lda #$03
	sta AUDV0
	lda #$08
	sta AUDC0
	lda #$1f
	sta AUDF0
	rts

Buzz
	lda #$04
	sta AUDV1
	lda #$06
	sta AUDC1
	lda #$10
	sta AUDF1
	rts

Silent
	lda #$0
	sta AUDV0
	sta AUDV1
	rts
	
;
; Graphics are placed so that the extra cycle in the PFData,X indexes
; is NEVER taken, by making sure it never has to index across a page
; boundary.  This way our cycle count holds true.
;

	org $FE00
;
; This is the tricky part of drawing a playfield: actually
; drawing it.  Well, the display routine and all that binary
; math was a bit tricky, too, but still, listen up.
;
; Playfield data isn't stored the way most bitmaps are, even
; one-dimensional bitmaps.  We will use the left side of the
; screen only, knowing that the right side is either repeated
; or reflected from it.
;
; In PF0 and PF2, the most significant bit (bit 7) is on the RIGHT
; side.  In PF1, the most significant bit is on the LEFT side.  This
; means that relative to PF0 and PF2, PF1 has a reversed bit order.
; It's just really weird.
;
;    PF0  |     PF1       |      PF2
;  4 5 6 7|7 6 5 4 3 2 1 0|0 1 2 3 4 5 6 7
;
; This is important to remember when doing calculations on bytes intended
; for the PF registers.  Defender gives a good example of this.
;
; It will become necessary to write a program that makes this easier,
; because it is easy to become confused when dealing with this system.
;

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
