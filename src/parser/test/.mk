include make/base.mk

T := $T/_

base_tests := $(shell find data/res -name '*.res' -o -name '*.resi')
syntax_tests := $(shell find $(TMP)/t/deps/syntax/tests/parsing/grammar -name '*.res' -o -name '*.resi')

tested = $(patsubst %,$T/%/tested,$(base_tests) $(syntax_tests))
expected = $(patsubst %/tested,%/expected,$(tested))
actual = $(patsubst %/tested,%/actual,$(tested))

simple_tested = $(patsubst %/tested,%/simple/tested,$(tested))
simple_expected = $(patsubst %/tested,%/simple/expected,$(tested))
simple_actual = $(patsubst %/tested,%/simple/actual,$(tested))

$D: $(tested)
$D/simple: $(simple_tested)

exe := $D/main.exe

define cmd
$(DUNE_DIR)/$(exe): force $(KEYS)/deps/done
	dune build $(exe)

$(tested): $T/%/tested: $T/%/expected $T/%/actual
	@ diff $$^ > /dev/null || (code --diff $$^ && false)
	$$(touch_target)

$(actual): $T/%/actual: % $(DUNE_DIR)/$(exe)
	mkdir -p $$(dir $$@)
	dune exec $(exe) -- --parser pc --stats $$@ $$< > $$@.tmp 2> $$@.log || (code $$@.log && false)
	mv $$@.tmp $$@

$(expected): $T/%/expected: %
	mkdir -p $$(dir $$@)
	dune exec $(exe) -- --parser res $$< > $$@.tmp 2> $$@.log || (code $$@.log && false)
	mv $$@.tmp $$@

$(simple_tested): $T/%/simple/tested: $T/%/simple/expected $T/%/simple/actual
	diff $$^ > /dev/null || ( \
		code --diff $$^ \
		&& dune exec $(exe) -- --parser pc $$(patsubst $T/%/simple/tested,%,$$@) > $$(patsubst $T/%/simple/tested,$T/%/actual.tmp,$$@) 2> /dev/null \
		&& dune exec $(exe) -- --parser res $$(patsubst $T/%/simple/tested,%,$$@) > $$(patsubst $T/%/simple/tested,$T/%/expected.tmp,$$@) 2> /dev/null \
		&& code --diff $$(patsubst $T/%/simple/tested,$T/%/expected.tmp,$$@) $$(patsubst $T/%/simple/tested,$T/%/actual.tmp,$$@) \
		&& false \
	)
	$$(touch_target)

$(simple_actual): $T/%/simple/actual: % $(DUNE_DIR)/$(exe)
	mkdir -p $$(dir $$@)
	dune exec $(exe) -- --simplified --parser pc --stats $$@ $$< > $$@.tmp 2> $$@.log || (code $$@.log && false)
	mv $$@.tmp $$@

$(simple_expected): $T/%/simple/expected: % $T/%/expected
	mkdir -p $$(dir $$@)
	dune exec $(exe) -- --simplified --parser res $$< > $$@.tmp
	mv $$@.tmp $$@
endef

$(eval $(cmd))
