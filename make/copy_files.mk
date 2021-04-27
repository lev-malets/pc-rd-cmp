
TMP_DIR := $(TMP_DIR)/_

src_files := $(patsubst $(DIR)/%,$(TMP_DIR)/%,$(shell find $(DIR) -maxdepth 1 -mindepth 1 ! -name '_.mk'))
dep_files := $(patsubst $(DIR)/%,$(TMP_DIR)/%,$(shell find $(DIR) ! -name '_.mk'))

define cmd
$(src_files): $(TMP_DIR)/%: | $(DIR)/%
	mkdir -p $$(dir $$@)
	ln -s $(WDIR)/$$(patsubst $(TMP_DIR)/%,$(DIR)/%,$$@) $$@
endef

$(eval $(cmd))
