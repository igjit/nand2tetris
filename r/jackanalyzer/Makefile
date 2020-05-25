JACKS = $(wildcard tests/testthat/data/*/*.jack)
VMS = $(JACKS:.jack=.vm)

vm: $(VMS)

%.vm: %.jack
	../../tools/JackCompiler.sh $^

clean:
	rm -f $(VMS)

.PHONY: vm clean
