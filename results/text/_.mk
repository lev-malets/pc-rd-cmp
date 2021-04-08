include make/base.mk

$(DONE): $(TMP_DIR)/repo/main.pdf

tex_files := $(patsubst $(DIR)/%,$(TMP_DIR)/repo/%,$(shell find $(DIR) -name '*.tex'))

define cmd
$(TMP_DIR)/template_done:
	$(call clone_repo, https://github.com/Kakadu/matmex-diploma-template, master, dfa74307f5c43905ecab56c11c6c069923bb6e5b)

	touch $$@
endef

$(TMP_DIR)/repo/main.pdf: $(tex_files) $(TMP_DIR)/repo/bib.bib
	cd $(dir $@) \
	&& xelatex main.tex \
	&& bibtex main.aux \
	&& xelatex main.tex \
	&& xelatex main.tex

$(TMP_DIR)/repo/%.tex: $(DIR)/%.tex
	cp -f $< $@
$(TMP_DIR)/repo/bib.bib: $(DIR)/bib.bib
	cp -f $< $@

$(eval $(cmd))
