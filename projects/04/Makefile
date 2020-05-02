ASMS = $(wildcard */*.asm)
HACKS = $(ASMS:.asm=.hack)
OUTS = $(ASMS:.asm=.out)

hack: $(HACKS)

%.hack: %.asm
	../../tools/Assembler.sh $^

%.out: %.hack
	../../tools/CPUEmulator.sh $(subst .hack,.tst,$^)

clean:
	rm -f $(HACKS) $(OUTS)

.PHONY: hack clean
