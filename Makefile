DUNE_DIR := _build/default

.DEFAULT_GOAL = build


f_exe_deps = $(wildcard $(1)/*.ml) $(wildcard $(1)/*.mli) $(wildcard $(1)/dune) dune-workspace

xelatex := xelatex main.tex

#----------------------------------------#

%/all:
	mkdir -p $(dir $@)
	cat $^ > $@

#----------------------------------------#

build: deps
	dune build

clean-top:
	rm -rf _build
	rm -rf tmp

.PHONY: build

include make-helper/main.mk

clean: clean-top
