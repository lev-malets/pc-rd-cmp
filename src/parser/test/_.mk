include make/base.mk

base_tests := $(wildcard data/res/*)
syntax_tests := \
	$(shell find $(TMP)/deps/syntax/repo/tests/parsing/grammar/signature \( \( -name '*.js' -o -name '*.res' \) -and ! -name '*.spec.js' \))

tests := $(patsubst %,$(TMP_DIR)/%/done,$(base_tests) $(syntax_tests))
tests_expected = $(patsubst %/done,%/sig/expected,$(tests))
tests_actual = $(patsubst %/done,%/sig/actual,$(tests))

exe := $(DIR)/main.exe

$(DUNE_TMP)/$(exe):
	dune build $(exe)

pc.test: $(tests)

$(tests): %/done: %/sig/expected %/sig/actual
	diff $^ > /dev/null || (code --diff $^ && false)
	$(touch_target)

$(tests_expected): $(TMP_DIR)/%/sig/expected: % | build
	mkdir -p $(dir $@)
	dune exec $(exe) res signature $< > $@.tmp
	mv $@.tmp $@
$(tests_actual): $(TMP_DIR)/%/sig/actual: % $(DUNE_DIR)/$(DIR)/main.exe | build
	mkdir -p $(dir $@)
	dune exec $(exe) pc signature $< > $@.tmp
	mv $@.tmp $@
