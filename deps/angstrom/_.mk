$(eval $(base))

define cmd
$(DONE):
	git clone https://github.com/inhabitedtype/angstrom -b master $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout ac93f6f3e9104f8ebfadab2f197601df5f92053a 2> /dev/null

	rm -rf $(TMP_DIR)/repo/lib_test

	cat $(TMP_DIR)/repo/benchmarks/pure_benchmark.ml \
	| sed 's/parse_bigstring/parse_bigstring ~consume:Prefix/' \
	| sed 's/let main () =/let main () = let open Filename in let path = dirname Sys.argv.(0) in/' \
	| sed 's/\"benchmarks/@@ path ^ \"/' \
	> $(TMP_DIR)/repo/benchmarks/pure_benchmark.ml.tmp
	mv -f $(TMP_DIR)/repo/benchmarks/pure_benchmark.ml.tmp $(TMP_DIR)/repo/benchmarks/pure_benchmark.ml

	touch $(DONE)
endef

$(eval $(cmd))
