include make/base.mk
include make/copy_files.mk

$(DONE): $(TMP_DIR)/main.pdf

$(TMP_DIR)/matmex-diploma-custom.cls:
	mkdir -p $(dir $@)
	wget \
		https://raw.githubusercontent.com/Kakadu/matmex-diploma-template/dfa74307f5c43905ecab56c11c6c069923bb6e5b/matmex-diploma-custom.cls \
		-O $@.tmp
	mv $@.tmp $@

$(TMP_DIR)/main.pdf: $(src_files) $(dep_files) $(TMP_DIR)/matmex-diploma-custom.cls
	cd $(dir $@) \
	&& $(xelatex) \
	&& bibtex main.aux \
	&& $(xelatex) \
	&& $(xelatex)
