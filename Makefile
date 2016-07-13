emacs ?= emacs

BASEDIR := $(shell pwd)

install:
	cd $(BASEDIR)
	$(emacs) -batch -l packages.el

.PHONY: install
