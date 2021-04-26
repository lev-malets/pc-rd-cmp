
WDIR := $(abspath .)
TMP := tmp
DUNE_DIR := _build/default

.DEFAULT_GOAL = build

touch_target = mkdir -p $(WDIR)/$(dir $@) && touch $(WDIR)/$@

path0 = $(notdir $(patsubst %/,%,$@))
path1 = $(notdir $(patsubst %/,%,$(dir $(patsubst %/,%,$@))))
path2 = $(notdir $(patsubst %/,%,$(dir $(patsubst %/,%,$(dir $(patsubst %/,%,$@))))))

f_exe_deps = $(wildcard $(1)/*.ml) $(wildcard $(1)/*.mli) $(wildcard $(1)/dune) dune-workspace

define clone_repo
	rm -rf $(TMP_DIR)/repo
	git clone $1 -b $2 $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout $3 2> /dev/null
endef

xelatex := xelatex main.tex

#----------------------------------------#

force: ;

%: %/default
	@ true $^

%/all:
	mkdir -p $(dir $@)
	cat $^ > $@

#----------------------------------------#

build: deps switch
	dune build

.PHONY: build

include $(shell find . -mindepth 2 -maxdepth 2 -name _.mk)
