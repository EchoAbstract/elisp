.PHONY: test compile clean

OUT := $(patsubst %.el,%.out,$(wildcard *.el))
ELC := $(patsubst %.el,%.elc,$(wildcard *.el))

%.elc: %.el
	emacs -Q -batch -L `pwd` -f batch-byte-compile $<

%.out: %.el
	emacs -Q -batch -l ert -l $< -f ert-run-tests-batch-and-exit
#	emacs -Q -batch -l ert -f ert-summarize-tests-batch-and-exit $<


test: $(OUT)

compile: $(ELC)

clean:
	rm $(ELC)
#	rm $(OUT)
