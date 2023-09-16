
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
	$(RM) FM_1.0.tar.tar.gz
	$(RM) src/*.o
	$(RM) src/*.so

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD INSTALL FM_1.0.tar.gz

.PHONY: uninstall
uninstall:
	$(R) CMD REMOVE FM || true


.PHONY: check
check:
	$(R) CMD check FM_1.0.tar.gz --ignore-vignettes --no-stop-on-test-error --no-manual

.PHONY: revision
revision:
	echo "Revision: $(shell git rev-parse HEAD)" >> DESCRIPTION
