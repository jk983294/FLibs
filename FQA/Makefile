
R ?= R

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	#$(MAKE) check

.PHONY: some
some:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install

.PHONY: clean
clean:
	$(RM) FQA_1.0.tar.tar.gz
	$(RM) src/*.o
	$(RM) src/*.so

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD INSTALL FQA_1.0.tar.gz

.PHONY: uninstall
uninstall:
	$(R) CMD REMOVE FQA || true


.PHONY: check
check:
	$(R) CMD check FQA_1.0.tar.gz --ignore-vignettes --no-stop-on-test-error --no-manual

.PHONY: revision
revision:
	echo "Revision: $(shell git rev-parse HEAD)" >> DESCRIPTION
