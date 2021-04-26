include make/base.mk

TMP_DIR := $(TMP_DIR)/_

exe := $(DIR)/exec.exe

tests_dir := $(TMP)/$(dir $(DIR))test/_

tested := $(shell test -d $(tests_dir)/str && find $(tests_dir)/str -wholename '*/tested')
files := $(patsubst $(tests_dir)/%/tested,$(TMP_DIR)/%/data,$(tested))
files_res := $(patsubst %/data,%/res,$(files))
files_pc := $(patsubst %/data,%/pc,$(files))

$(DIR): $(files)

$(files): $(TMP_DIR)/%/data: $(TMP_DIR)/%/res $(TMP_DIR)/%/pc
	cat $^ > $@

define cmd
$(files_res): $(TMP_DIR)/%/res: $(tests_dir)/%/expected | build
	mkdir -p $$(dir $$@)
	dune exec $(exe) res signature $$(patsubst $(TMP_DIR)/str/%/res,$(TMP)/deps/syntax/repo/%,$$@) > $$@.tmp
	mv $$@.tmp $$@

$(files_pc): $(TMP_DIR)/%/pc: $(tests_dir)/%/actual | build
	mkdir -p $$(dir $$@)
	dune exec $(exe) pc signature $$(patsubst $(TMP_DIR)/str/%/pc,$(TMP)/deps/syntax/repo/%,$$@) > $$@.tmp
	mv $$@.tmp $$@
endef

$(eval $(cmd))
