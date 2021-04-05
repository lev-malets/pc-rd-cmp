$(eval $(base))

$(DONE): \
	$(DIR)/lscpu \
	$(DIR)/angstrom \
	$(DIR)/angstrom-mod

$(DIR)/lscpu:
	lscpu > $@
$(DIR)/json: $(TMP)/src/json/bench/all
	cp $< $@

angstrom_benchmarks := \
	characters \
	http \
	http-version \
	json \
	loops \
	numbers \
	endian \
	short-strings

angstrom_mod_data := $(patsubst %,$(TMP_DIR)/angstrom-mod/data/%,$(angstrom_benchmarks))
angstrom_data := $(patsubst %,$(TMP_DIR)/angstrom/data/%,$(angstrom_benchmarks))

$(DIR)/angstrom: $(TMP_DIR)/angstrom/all
	cp $< $@
$(DIR)/angstrom-mod: $(TMP_DIR)/angstrom-mod/all
	cp $< $@

$(TMP_DIR)/angstrom-mod/all: $(angstrom_mod_data)
$(TMP_DIR)/angstrom/all: $(angstrom_data)

$(angstrom_mod_data): $(TMP_DIR)/angstrom-mod/data/%: | build
	mkdir -p $(dir $@)
	dune exec --release src/angstrom-mod/benchmarks/pure_benchmark.exe $(notdir $@) > $@
	echo >> $@
	dune exec --release src/angstrom-mod/benchmarks/pure_benchmark.exe $(notdir $@) >> $@
	echo >> $@

$(angstrom_data): $(TMP_DIR)/angstrom/data/%: | build
	mkdir -p $(dir $@)
	dune exec --release $(TMP)/deps/angstrom/repo/benchmarks/pure_benchmark.exe $(notdir $@) > $@
	echo >> $@
	dune exec --release $(TMP)/deps/angstrom/repo/benchmarks/pure_benchmark.exe $(notdir $@) >> $@
	echo >> $@
