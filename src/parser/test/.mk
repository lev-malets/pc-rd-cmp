include make/base.mk

T := $T/_

tested := $(patsubst %,$T/%/tested,$(base_files) $(syntax_test_files) $(syntax_benchmark_files))
expected := $(patsubst %/tested,%/expected,$(tested))
actual := $(patsubst %/tested,%/actual,$(tested))

$D: $(tested)

exe := $D/main.exe

define cmd
$(DUNE_DIR)/$(exe): force $(KEYS)/deps/done
	$(call dune,build $(exe))

$(tested): $T/%/tested: % $(DUNE_DIR)/$(exe) $D/exec.sh
	@ mkdir -p $$(dir $$@)
	@ echo Test $$<
	@ bash ./$D/exec.sh $$< $$(dir $$@)
	@ touch $$@
	@ echo Ok
endef

$(eval $(cmd))
