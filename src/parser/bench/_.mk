include make/base.mk

exe := $(DIR)/exec.exe

tested := $(shell find $(dir $(TMP_DIR))test/tests -wholename '*/sig/tested')
files := $(patsubst $(dir $(TMP_DIR))test/tests/%/sig/tested,$(TMP_DIR)/%/data,$(tested))
files_res := $(patsubst %/data,%/res,$(files))
files_pc := $(patsubst %/data,%/pc,$(files))

$(DIR): $(files)

$(files): $(TMP_DIR)/%/data: $(TMP_DIR)/%/res $(TMP_DIR)/%/pc
	cat $^ > $@

define cmd
$(files_res): $(TMP_DIR)/%/res: $(dir $(TMP_DIR))test/tests/%/sig/expected | build
	mkdir -p $$(dir $$@)
	dune exec $(exe) res signature $$(patsubst $(TMP_DIR)/%/res,$(TMP)/deps/syntax/repo/tests/%,$$@) > $$@.tmp 2> /dev/null
	dune exec $(exe) res signature $$(patsubst $(TMP_DIR)/%/res,$(TMP)/deps/syntax/repo/tests/%,$$@) >> $$@.tmp 2> /dev/null
	mv $$@.tmp $$@

$(files_pc): $(TMP_DIR)/%/pc: $(dir $(TMP_DIR))test/tests/%/sig/actual | build
	mkdir -p $$(dir $$@)
	dune exec $(exe) pc signature $$(patsubst $(TMP_DIR)/%/pc,$(TMP)/deps/syntax/repo/tests/%,$$@) > $$@.tmp 2> /dev/null
	dune exec $(exe) pc signature $$(patsubst $(TMP_DIR)/%/pc,$(TMP)/deps/syntax/repo/tests/%,$$@) >> $$@.tmp 2> /dev/null
	mv $$@.tmp $$@
endef

$(eval $(cmd))
