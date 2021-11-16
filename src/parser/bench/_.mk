include make/base.mk

TMP_DIR := $T/_

exe := $D/exec.exe

tests_dir := $(TMP)/$(dir $D)test/_

tested := $(shell test -d $(tests_dir)/str && find $(tests_dir)/str -wholename '*/tested')
files := $(patsubst $(tests_dir)/%/tested,$T/%/data,$(tested))
files_res := $(patsubst %/data,%/res,$(files))
files_pc := $(patsubst %/data,%/pc,$(files))

$D: $(files)

$(files): $T/%/data: $T/%/res $T/%/pc
	cat $^ > $@

define cmd
$(files_res): $T/%/res: $(tests_dir)/%/expected | build
	mkdir -p $$(dir $$@)
	dune exec $(exe) res signature $$(patsubst $T/str/%/res,$(TMP)/deps/syntax/repo/%,$$@) > $$@.tmp
	mv $$@.tmp $$@

$(files_pc): $T/%/pc: $(tests_dir)/%/actual | build
	mkdir -p $$(dir $$@)
	dune exec $(exe) pc signature $$(patsubst $T/str/%/pc,$(TMP)/deps/syntax/repo/%,$$@) > $$@.tmp
	mv $$@.tmp $$@
endef

$(eval $(cmd))
