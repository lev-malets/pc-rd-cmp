base_files := $(shell find data/res -name '*.res' -o -name '*.resi')
syntax_test_files := $(shell find $(TMP_DIR)/t/deps/syntax/tests/parsing/grammar -name '*.res' -o -name '*.resi')
syntax_benchmark_files := $(shell find $(TMP_DIR)/t/deps/syntax/benchmarks/data -name '*.res' -o -name '*.resi')
