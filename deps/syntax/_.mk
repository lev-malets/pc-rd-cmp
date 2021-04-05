include $(eval $(base))

define cmd
$(DONE):
	git clone https://github.com/rescript-lang/syntax -b master $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout 07421c7a15635deb3c16a6c1892be6772a3b0939 2> /dev/null

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
