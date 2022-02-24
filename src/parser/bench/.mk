T := $T/_

tested := $(patsubst %,$T/file/%/tested,$(syntax_benchmark_files))
metrics := $(patsubst $T/file/%/tested,$T/file/%/metrics,$(tested))

$K/done: $T/stats

exe := $D/main.exe

_build/default/$(exe): force $(KEYS)/deps/done
	$(log_err dune build $(exe))

$T/stats: $D/avg.sh $(metrics)
	. $^ > $@.tmp
	mv $@.tmp $@

$T/file/%/metrics: $T/file/%/tested $D/metrics.sh
	. $D/metrics.sh $< > $@

$(tested): $T/file/%/tested: % _build/default/$(exe) $D/exec.sh
	mkdir -p $(dir $@)

	$(log_err dune exec $(exe) -- --input $<) > $@.tmp
	mv $@.tmp $@
