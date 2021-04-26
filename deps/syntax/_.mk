include make/base.mk

define cmd
$(DONE):
	$(call clone_repo, https://github.com/rescript-lang/syntax, master, 1680ae5d0e5546f5a150e028a148aeee257bcd20)

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
