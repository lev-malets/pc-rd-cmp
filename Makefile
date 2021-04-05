
WDIR := $(abspath .)
TMP := tmp
DUNE_DIR := _build/default

.DEFAULT_GOAL = build

touch_target = mkdir -p $(WDIR)/$(dir $@) && touch $(WDIR)/$@

path0 = $(notdir $(patsubst %/,%,$@))
path1 = $(notdir $(patsubst %/,%,$(dir $(patsubst %/,%,$@))))
path2 = $(notdir $(patsubst %/,%,$(dir $(patsubst %/,%,$(dir $(patsubst %/,%,$@))))))

f_exe_deps = $(wildcard $(1)/*.ml) $(wildcard $(1)/*.mli) $(wildcard $(1)/dune) dune-workspace

define base
DIR := $(patsubst %/,%,$(dir $(lastword $(MAKEFILE_LIST))))
TMP_DIR := $(TMP)/$$(DIR)
DIR_ABS := $(WDIR)/$$(DIR)
TMP_DIR_ABS := $(WDIR)/$$(TMP_DIR)
DONE := $$(TMP_DIR)/done
CLEAN := $$(DIR)/clean
CLEAN_DEFAULT := $$(CLEAN)/default
DONE_DEFAULT := $$(DONE)/default
RE := re.$$(DIR)
SUB := $$(shell find $$(DIR) -mindepth 2 -maxdepth 2 -name _.mk)
SUB_DIRS := $$(patsubst %/_.mk,%,$$(SUB))

clean: $$(CLEAN)
re: $$(RE)

$$(RE): $$(CLEAN) $$(DIR)
$$(DIR): $$(SUB_DIRS) $$(DONE)

define cmd
$$(CLEAN_DEFAULT):
	rm -rf $$(TMP_DIR)
$$(DONE_DEFAULT):
	@ true
endef
$$(eval $$(cmd))
endef # base

#----------------------------------------#

force: ;

%: %/default
	true $^

%/all:
	mkdir -p $(dir $@)
	cat $^ > $@

#----------------------------------------#

build: $(TMP)/deps/done $(TMP)/switch/done
	dune build

.PHONY: build

include $(shell find . -mindepth 2 -maxdepth 2 -name _.mk)
