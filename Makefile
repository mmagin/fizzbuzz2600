TARGET := fizzbuzz.bin

all: $(TARGET)
	z26 $(TARGET)

%.bin: %.asm
	dasm $< -f3 -o$@

clean:
	rm -f $(TARGET)
