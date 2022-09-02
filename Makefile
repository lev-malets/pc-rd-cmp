DUNE_DIR := _build/default

.DEFAULT_GOAL = build

#----------------------------------------#

deps: $T/.done.deps

fmt: deps
	tmp/deps/treefmt/treefmt

build: deps
	mkdir -p tmp
	dune build

clean-top:
	rm -rf _build
	rm -rf tmp

.PHONY: build deps fmt

include make-helper/main.mk

clean: clean-top

$T/.done.deps: force
	bash scripts/deps/clone.sh tmp
