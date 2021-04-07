include make/base.mk

define cmd
$(DONE):
	$(call clone_repo, https://github.com/murmour/mparser, master, 826fc1d9c68b5cafc0b2637ad395226f1a5abfa0)

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	touch $(DONE)
endef

$(eval $(cmd))
