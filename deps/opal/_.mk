$(eval $(base))

define cmd
$(DONE):
	git clone https://github.com/pyrocat101/opal -b master $(TMP_DIR)/repo
	git -C $(TMP_DIR)/repo checkout ac495a4fc141cf843da74d223baecca47324acd4 2> /dev/null

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	rm $(TMP_DIR)/repo/opal.mli

	touch $(DONE)
endef

$(eval $(cmd))
