$K/done: $K/tested

_build/default/$D/main.exe: force $(KEYS)/deps/done
	$(log_err dune build $D/main.exe)

$K/tested: _build/default/$D/main.exe $D/exec.sh
	@ mkdir -p $(dir $@)
	@ bash ./$D/exec.sh $T
	@ touch $@
