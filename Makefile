TARGET := fizzbuzz.bin

all: $(TARGET)

%.bin: %.asm
	dasm $< -f3 -o$@

clean:
	rm -f $(TARGET)
