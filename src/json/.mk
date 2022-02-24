LIBS := $(patsubst $D/libs/p_%.ml,%,$(wildcard $D/libs/p_*))

TEST_TARGETS := $(addprefix json.test.,$(LIBS))
BENCH_TARGETS := $(addprefix json.bench.,$(LIBS))

json.test: $(TEST_TARGETS)
json.bench: $(BENCH_TARGETS)

$(TEST_TARGETS): json.test.%: $(KEYS)/deps/done $T/test/%/dune $T/test/%/main.ml
	dune exec $T/test/$(patsubst json.test.%,%,$@)/main.exe

$(BENCH_TARGETS): json.bench.%: $(KEYS)/deps/done $T/bench/%/dune $T/bench/%/main.ml
	dune exec $T/bench/$(patsubst json.bench.%,%,$@)/main.exe

$T/test/%/main.ml:
	mkdir -p $(dir $@)
	echo "module M = P_test.Make(P_$(path1).Parser);; include M" > $@
$T/test/%/dune:
	mkdir -p $(dir $@)
	echo "(executable (name main) (libraries p_$(path1) p_test))" > $@

$T/bench/%/main.ml:
	mkdir -p $(dir $@)
	echo "module M = P_bench.Make(P_$(path1).Parser);; include M" > $@
$T/bench/%/dune:
	mkdir -p $(dir $@)
	echo "(executable (name main) (libraries p_$(path1) p_bench))" > $@

.PHONY: $(BENCH_TARGETS) $(TEST_TARGETS) json.test json.bench
.INTERMEDIATE: \
	$(patsubst %,$T/test/%/main.ml,$(LIBS)) \
	$(patsubst %,$T/test/%/dune,$(LIBS)) \
	$(patsubst %,$T/bench/%/main.ml,$(LIBS)) \
	$(patsubst %,$T/bench/%/dune,$(LIBS))
