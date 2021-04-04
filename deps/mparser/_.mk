$(eval $(base))

define cmd
$(DONE):
	git clone https://github.com/murmour/mparser -b master $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout 826fc1d9c68b5cafc0b2637ad395226f1a5abfa0 2> /dev/null

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
