
WDIR := $(abspath .)
TMP := tmp
DUNE_DIR := _build/default

.DEFAULT_GOAL = $(TMP)/built

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
CLEAN := clean.$$(DIR)
SUB := $$(shell echo $$(DIR)/*/_.mk)

clean: $$(CLEAN)
.PHONY: $$(CLEAN) $$(DIR)

define cmd
$$(CLEAN):
	rm -rf $$(TMP_DIR)

$$(DIR): $$(SUB)
endef
$$(eval $$(cmd))
endef # base

phony:
.PHONY: phony

include $(shell echo */_.mk)

$(TMP)/built: phony $(TMP)/deps/done $(TMP)/switch/done
	dune build
	touch $@
