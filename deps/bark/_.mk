$(eval $(base))

define cmd
$(DONE):
	git clone https://github.com/justinlubin/bark -b master $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout 8586a2796361d54009c2f942e24fd5aad71f1cd0 2> /dev/null

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
