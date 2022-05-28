$K/done: $K/tested

_build/default/$D/test.exe: force $(KEYS)/deps/done
	dune build $D/test.exe

$K/tested: _build/default/$D/test.exe
	$< test -c noloc
	$(touch)
