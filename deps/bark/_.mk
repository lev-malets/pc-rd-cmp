include make/base.mk

define cmd
$(DONE):
	$(call clone_repo, https://github.com/justinlubin/bark, master, 8586a2796361d54009c2f942e24fd5aad71f1cd0)

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
