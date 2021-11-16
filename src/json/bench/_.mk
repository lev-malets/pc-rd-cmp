include make/base.mk

data_files := $(patsubst data/json/%,$T/data/%, \
	$(wildcard data/json/complex/*) \
	$(wildcard data/json/others/*) \
	$(wildcard data/json/ws/*))

$T/all: $(data_files)

define cmd
$(data_files): $T/data/%: data/json/% | build
	mkdir -p $$(dir $$@)
	echo "$$(path1):$$(path0)" > $$@.tmp
	dune exec --release $D/main.exe "$$(path1):$$(path0)" >> $$@.tmp 2> /dev/null
	echo >> $$@.tmp
	mv $$@.tmp $$@
endef

$(eval $(cmd))
