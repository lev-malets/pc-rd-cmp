include make/base.mk

TMP_DIR := $(TMP_DIR)/_

commit := 0090035986ddbcb91175195470d0e9d2edc632ab

define cmd
$(TMP_DIR)/cloned:
	$(call clone_repo, https://github.com/rescript-lang/ocaml, 4.06.1+BS, $(commit))
	touch $$@

$(TMP_DIR)/changed: $(TMP_DIR)/cloned $(shell find $(DIR)/_changes)
	cp -rf $(DIR)/_changes/* $(TMP_DIR)/repo/
	touch $$@
endef

$(DONE): $(TMP_DIR)/changed
	touch $@

$(eval $(cmd))
