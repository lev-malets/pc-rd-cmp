$(eval $(base))

define cmd
$(DONE):
	git clone https://github.com/inhabitedtype/angstrom -b master $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout ac93f6f3e9104f8ebfadab2f197601df5f92053a 2> /dev/null

	rm -rf $(TMP_DIR)/repo/lib_test
	rm -rf $(TMP_DIR)/repo/benchmarks

	touch $(DONE)
endef

$(eval $(cmd))
