; Some macros of my own

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
