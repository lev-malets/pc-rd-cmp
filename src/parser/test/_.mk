include make/base.mk

TMP_DIR := $(TMP_DIR)/_

base_tests := $(wildcard data/res/*)
signature_tests := $(shell find $(TMP)/deps/syntax/repo/tests/parsing/grammar -name '*.resi')
structure_tests := $(shell find $(TMP)/deps/syntax/repo/tests/parsing/grammar/signature -name '*.res')

signature_tests := $(patsubst $(TMP)/deps/syntax/repo/%,$(TMP_DIR)/sig/%/tested,$(signature_tests))
signature_tests_expected = $(patsubst %/tested,%/expected,$(signature_tests))
signature_tests_actual = $(patsubst %/tested,%/actual,$(signature_tests))

structure_tests := $(patsubst $(TMP)/deps/syntax/repo/%,$(TMP_DIR)/str/%/tested,$(structure_tests))
structure_tests_expected = $(patsubst %/tested,%/expected,$(structure_tests))
structure_tests_actual = $(patsubst %/tested,%/actual,$(structure_tests))

base_tests := $(patsubst data/res/%,$(TMP_DIR)/sig/base_test/%/tested,$(base_tests))
base_tests_expected = $(patsubst %/tested,%/expected,$(base_tests))
base_tests_actual = $(patsubst %/tested,%/actual,$(base_tests))

exe := $(DIR)/main.exe

$(DUNE_TMP)/$(exe):
	dune build $(exe)

$(DIR): $(base_tests) $(signature_tests) $(structure_tests)

$(TMP_DIR)/%/tested: $(TMP_DIR)/%/expected $(TMP_DIR)/%/actual
	diff $^ > /dev/null || (cat $(dir $@)actual.log && code --diff $^ && false)
	$(touch_target)

$(TMP_DIR)/sig/%/actual: | build
	mkdir -p $(dir $@)
	dune exec $(exe) pc signature $< > $@.tmp 2> $@.log || (cat $@.log && false)
	mv $@.tmp $@

$(TMP_DIR)/sig/%/expected: | build
	mkdir -p $(dir $@)
	dune exec $(exe) res signature $< > $@.tmp 2> /dev/null
	mv $@.tmp $@

$(TMP_DIR)/str/%/actual: | build
	mkdir -p $(dir $@)
	dune exec $(exe) pc structure $< > $@.tmp 2> $@.log || (cat $@.log && false)
	mv $@.tmp $@

$(TMP_DIR)/str/%/expected: | build
	mkdir -p $(dir $@)
	dune exec $(exe) res structure $< > $@.tmp 2> /dev/null
	mv $@.tmp $@

$(base_tests_expected): $(TMP_DIR)/sig/base_test/%/expected: data/res/%
$(base_tests_actual): $(TMP_DIR)/sig/base_test/%/actual: data/res/% $(DUNE_DIR)/$(DIR)/main.exe

$(signature_tests_expected): $(TMP_DIR)/sig/%/expected: $(TMP)/deps/syntax/repo/%
$(signature_tests_actual): $(TMP_DIR)/sig/%/actual: $(TMP)/deps/syntax/repo/% $(DUNE_DIR)/$(DIR)/main.exe

$(structure_tests_expected): $(TMP_DIR)/str/%/expected: $(TMP)/deps/syntax/repo/%
$(structure_tests_actual): $(TMP_DIR)/str/%/actual: $(TMP)/deps/syntax/repo/% $(DUNE_DIR)/$(DIR)/main.exe

$(TMP_DIR)/sig/tests/%/expected: $(DIR)/expected/%/sig
	mkdir -p $(dir $@)
	cp -f $< $@
