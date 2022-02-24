data_files := $(patsubst data/json/%,$T/data/%, \
	$(wildcard data/json/complex/*) \
	$(wildcard data/json/others/*) \
	$(wildcard data/json/ws/*))

$K/done: $(data_files)

_build/default/$D/main.exe: force $(KEYS)/deps/done
	$(log_err dune build $D/main.exe)

$(data_files): $T/data/%: data/json/% _build/default/$D/main.exe
	mkdir -p $(dir $@)
	echo "$(path1):$(path0)" > $@.tmp
	dune exec --release $D/main.exe "$(path1):$(path0)" >> $@.tmp 2> /dev/null
	echo >> $@.tmp
	mv $@.tmp $@
