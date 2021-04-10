include make/base.mk

base_tests := $(wildcard data/res/*)
syntax_tests := \
	$(shell find $(TMP)/deps/syntax/repo/tests/parsing/grammar/signature \( \( -name '*.js' -o -name '*.res' \) -and ! -name '*.spec.js' \))

tests := $(patsubst $(TMP)/deps/syntax/repo/%,$(TMP_DIR)/%/sig/tested,$(syntax_tests))
tests_expected = $(patsubst %/sig/tested,%/sig/expected,$(tests))
tests_actual = $(patsubst %/sig/tested,%/sig/actual,$(tests))

base_tests := $(patsubst data/res/%,$(TMP_DIR)/base_test/%/sig/tested,$(base_tests))
base_tests_expected = $(patsubst %/sig/tested,%/sig/expected,$(base_tests))
base_tests_actual = $(patsubst %/sig/tested,%/sig/actual,$(base_tests))

exe := $(DIR)/main.exe

$(DUNE_TMP)/$(exe):
	dune build $(exe)

$(DIR): $(base_tests) $(tests)

$(TMP_DIR)/%/sig/tested: $(TMP_DIR)/%/sig/expected $(TMP_DIR)/%/sig/actual
	diff $^ > /dev/null || (code --diff $^ && false)
	$(touch_target)

$(TMP_DIR)/%/sig/actual:
	mkdir -p $(dir $@)
	dune exec $(exe) pc signature $< > $@.tmp
	mv $@.tmp $@

$(TMP_DIR)/%/sig/expected:
	mkdir -p $(dir $@)
	dune exec $(exe) res signature $< > $@.tmp
	mv $@.tmp $@

$(base_tests_expected): $(TMP_DIR)/base_test/%/sig/expected: data/res/%
$(base_tests_actual): $(TMP_DIR)/base_test/%/sig/actual: data/res/% $(DUNE_DIR)/$(DIR)/main.exe

$(tests_expected): $(TMP_DIR)/tests/%/sig/expected: $(TMP)/deps/syntax/repo/tests/% | build
$(tests_actual): $(TMP_DIR)/tests/%/sig/actual: $(TMP)/deps/syntax/repo/tests/% $(DUNE_DIR)/$(DIR)/main.exe | build

$(TMP_DIR)/tests/%/sig/expected: $(DIR)/expected/%/sig
	mkdir -p $(dir $@)
	cp -f $< $@
