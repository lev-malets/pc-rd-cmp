DIR := $(patsubst %/,%,$(dir $(shell echo $(MAKEFILE_LIST) | awk '{print $$(NF - 1)}')))
TMP_DIR := $(TMP)/$(DIR)
DIR_ABS := $(WDIR)/$(DIR)
TMP_DIR_ABS := $(WDIR)/$(TMP_DIR)
DONE := $(TMP_DIR)/done
CLEAN := $(DIR)/clean
CLEAN_DEFAULT := $(CLEAN)/default
DONE_DEFAULT := $(DONE)/default
RE := re.$(DIR)
SUB := $(shell find $(DIR) -mindepth 2 -maxdepth 2 -name _.mk)
SUB_DIRS := $(patsubst %/_.mk,%,$(SUB))

.PHONY: $(DIR)

clean: $(CLEAN)
re: $(RE)

$(RE): $(CLEAN) $(DIR)
$(DIR): $(SUB_DIRS) $(DONE)

define cmd
$(CLEAN_DEFAULT):
	rm -rf $(TMP_DIR)
$(DONE_DEFAULT):
	@ true
endef

$(eval $(cmd))
