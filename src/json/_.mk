include make/base.mk

LIBS := $(patsubst $(DIR)/libs/p_%.ml,%,$(wildcard $(DIR)/libs/p_*))

TEST_TARGETS := $(addprefix json.test.,$(LIBS))
BENCH_TARGETS := $(addprefix json.bench.,$(LIBS))

json.test: $(TEST_TARGETS)
json.bench: $(BENCH_TARGETS)

define cmd
$(TEST_TARGETS): json.test.%: $(TMP)/deps/done $(TMP_DIR)/test/%/dune $(TMP_DIR)/test/%/main.ml
	dune exec $(TMP_DIR)/test/$$(patsubst json.test.%,%,$$@)/main.exe

$(BENCH_TARGETS): json.bench.%: $(TMP)/deps/done $(TMP_DIR)/bench/%/dune $(TMP_DIR)/bench/%/main.ml
	dune exec $(TMP_DIR)/bench/$$(patsubst json.bench.%,%,$$@)/main.exe
endef

$(eval $(cmd))

$(TMP_DIR)/test/%/main.ml:
	mkdir -p $(dir $@)
	echo "module M = P_test.Make(P_$(path1).Parser);; include M" > $@
$(TMP_DIR)/test/%/dune:
	mkdir -p $(dir $@)
	echo "(executable (name main) (libraries p_$(path1) p_test))" > $@

$(TMP_DIR)/bench/%/main.ml:
	mkdir -p $(dir $@)
	echo "module M = P_bench.Make(P_$(path1).Parser);; include M" > $@
$(TMP_DIR)/bench/%/dune:
	mkdir -p $(dir $@)
	echo "(executable (name main) (libraries p_$(path1) p_bench))" > $@

.PHONY: $(BENCH_TARGETS) $(TEST_TARGETS) json.test json.bench
.INTERMEDIATE: \
	$(patsubst %,$(TMP_DIR)/test/%/main.ml,$(LIBS)) \
	$(patsubst %,$(TMP_DIR)/test/%/dune,$(LIBS)) \
	$(patsubst %,$(TMP_DIR)/bench/%/main.ml,$(LIBS)) \
	$(patsubst %,$(TMP_DIR)/bench/%/dune,$(LIBS))

include $(shell echo $(DIR)/*/_.mk)
