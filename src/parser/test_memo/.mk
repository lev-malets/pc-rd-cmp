include make/base.mk

T := $T/_

tested := $(patsubst %,$T/%/tested,$(_base_files) $(syntax_test_files) $(syntax_benchmark_files))
memoized := $(patsubst %/tested,%/memoized,$(tested))
notmemoized := $(patsubst %/tested,%/notmemoized,$(tested))

$D: $(tested)

exe := $D/main.exe

define cmd
$(DUNE_DIR)/$(exe): force $(KEYS)/deps/done
	$(call dune,build $(exe))

$(tested): $T/%/tested: $T/%/notmemoized $T/%/memoized
	@ diff $$^ > /dev/null || (code --diff $$^ && false)
	touch $$@

$(notmemoized): $T/%/notmemoized: % $(DUNE_DIR)/$(exe)
	mkdir -p $$(dir $$@)
	$(call dune,exec $(exe) -- --input $$< --output $$@.tmp)
	mv $$@.tmp $$@

$(memoized): $T/%/memoized: % $(DUNE_DIR)/$(exe)
	mkdir -p $$(dir $$@)
	$(call dune,exec $(exe) -- --input $$< --output $$@.tmp --memo)
	mv $$@.tmp $$@
endef

$(eval $(cmd))
