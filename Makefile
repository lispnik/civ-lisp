SBCL ?= sbcl
HEAP ?= 4096

# Build a standalone executable: ./civ-lisp
civ-lisp: civ-lisp.asd src/*.lisp assets/torch.png
	$(SBCL) --dynamic-space-size $(HEAP) --non-interactive --load build.lisp

.PHONY: run deps clean
# Run from source without building an image
run:
	$(SBCL) --dynamic-space-size $(HEAP) --non-interactive --load run.lisp

# Fetch dependencies via ocicl
deps:
	ocicl install

clean:
	rm -f civ-lisp
	find . -name '*.fasl' -delete