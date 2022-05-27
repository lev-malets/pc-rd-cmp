$K/done: $K/core_bench_result $K/perf_result

main_exe := _build_release/default/$D/main.exe

$(main_exe): force $(KEYS)/deps/done
	dune build --build-dir _build_release --release $D/main.exe

$K/core_bench_result: $(main_exe)
	bash $D/exec_core_bench.sh $T/_cb_result $(main_exe)
	$(touch)

$K/perf_result: $(utils_exec_exe) $D/exec_perf.sh
	bash $D/exec_perf.sh $T/_perf_result $(utils_exec_exe)
	$(touch)

$D/core_bench: $K/core_bench_result
$D/perf: $K/perf_result

$T/stats: $D/avg.sh $(metrics)
	. $^ > $@.tmp
	mv $@.tmp $@

$T/file/%/metrics: $T/file/%/tested $D/metrics.sh
	. $D/metrics.sh $< > $@

$(tested): $T/file/%/tested: % _build/default/$(exe) $D/exec.sh
	mkdir -p $(dir $@)

	$(log_err dune exec $(exe) -- --input $<) > $@.tmp
	mv $@.tmp $@
