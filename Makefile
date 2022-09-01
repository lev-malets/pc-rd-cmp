DUNE_DIR := _build/default

.DEFAULT_GOAL = build

#----------------------------------------#

build: deps
	mkdir -p tmp
	dune build

clean-top:
	rm -rf _build
	rm -rf tmp

.PHONY: build

include make-helper/main.mk

clean: clean-top
