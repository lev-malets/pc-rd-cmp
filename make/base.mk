DIR := $(patsubst %/,%,$(dir $(shell echo $(MAKEFILE_LIST) | awk '{print $$(NF - 1)}')))
TMP_DIR := $(TMP)/$(DIR)
DIR_ABS := $(WDIR)/$(DIR)
TMP_DIR_ABS := $(WDIR)/$(TMP_DIR)
DONE := $(TMP_DIR)/done
CLEAN := $(DIR)/clean
RE := re.$(DIR)
SUB := $(shell find $(DIR) -mindepth 2 -maxdepth 2 -name _.mk)
SUB_DIRS := $(patsubst %/_.mk,%,$(SUB))

.PHONY: $(DIR)

clean: $(CLEAN)
re: $(RE)

$(RE): $(CLEAN) $(DIR)
$(DIR): $(SUB_DIRS) $(DONE)

define cmd
$(CLEAN)/default:
	rm -rf $(TMP_DIR)
$(DONE)/default:
	@ true
endef

$(eval $(cmd))
