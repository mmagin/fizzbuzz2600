
%.bin: %.asm
	dasm $< -f3 -o$@
