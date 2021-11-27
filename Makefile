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

.PHONY: build

dune = @ mkdir -p $$(dir $T/log/$$@.log) && dune $(1) 2> $T/log/$$@.log || (cat $T/log/$$@.log && false)

include make/main.mk
