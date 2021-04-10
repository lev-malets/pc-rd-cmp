include make/base.mk

$(DONE): $(TMP_DIR)/repo/main.pdf

tex_files := $(patsubst $(DIR)/%,$(TMP_DIR)/repo/%,$(shell find $(DIR) -name '*.tex' -o -name '*.bib'))

xelatex := xelatex -jobname=main main.tex

define cmd
$(TMP_DIR)/template_done:
	$(call clone_repo, https://github.com/Kakadu/matmex-diploma-template, master, dfa74307f5c43905ecab56c11c6c069923bb6e5b)

	touch $$@

$(tex_files): $(TMP_DIR)/repo/%: | $(DIR)/% $(TMP_DIR)/template_done
	ln -s $(WDIR)/$$(patsubst $(TMP_DIR)/repo/%,$(DIR)/%,$$@) $(TMP_DIR)/repo/
endef

$(TMP_DIR)/repo/main.pdf: $(tex_files)
	cd $(dir $@) \
	&& $(xelatex) \
	&& bibtex main.aux \
	&& $(xelatex) \
	&& $(xelatex)

$(eval $(cmd))
