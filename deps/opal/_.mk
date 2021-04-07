include make/base.mk

define cmd
$(DONE):
	$(call clone_repo, https://github.com/pyrocat101/opal, master, ac495a4fc141cf843da74d223baecca47324acd4)

	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/

	rm $(TMP_DIR)/repo/opal.mli

	touch $(DONE)
endef

$(eval $(cmd))
