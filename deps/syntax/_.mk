include make/base.mk

define cmd
$(DONE):
	$(call clone_repo, https://github.com/rescript-lang/syntax, master, 07421c7a15635deb3c16a6c1892be6772a3b0939)

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
