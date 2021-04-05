$(eval $(base))

data_files := $(patsubst data/json/%,$(TMP_DIR)/data/%, \
	$(wildcard data/json/complex/*) \
	$(wildcard data/json/others/*) \
	$(wildcard data/json/ws/*))

$(TMP_DIR)/all: $(data_files)

define cmd
$(data_files): $(TMP_DIR)/data/%: data/json/% | $(TMP)/built
	mkdir -p $$(dir $$@)
	echo "$$(path1):$$(path0)" > $$@.tmp
	dune exec --release $(DIR)/main.exe "$$(path1):$$(path0)" >> $$@.tmp
	echo >> $$@.tmp
	echo "$$(path1):$$(path0)" >> $$@.tmp
	dune exec --release $(DIR)/main.exe "$$(path1):$$(path0)" >> $$@.tmp
	mv $$@.tmp $$@
endef

$(eval $(cmd))
