ASMS = $(wildcard */*.asm)
HACKS = $(ASMS:.asm=.hack)

hack: $(HACKS)

%.hack: %.asm
	../../tools/Assembler.sh $^

clean:
	rm -f $(HACKS)

.PHONY: hack clean
