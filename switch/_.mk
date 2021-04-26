include make/base.mk

DEPS := \
	yojson \
	extlib \
	core_bench \
	alcotest \
	bigstringaf \
	async \
	lwt \
	fix \
	odoc \
	ppx_jane \
	ocaml-lsp-server \
	ppx_deriving \
	fmt
DEPS_TARGETS := $(addprefix $(TMP_DIR)/libs/, $(DEPS))

set_switch := eval $$(opam env --switch=$(TMP_DIR)/switch_bs --set-switch)

define cmd
$(DONE): $(DEPS_TARGETS)
	$$(touch_target)

$(DEPS_TARGETS): $(TMP_DIR)/libs/%: $(TMP_DIR)/bs_initialized
	$$(set_switch) && opam install $$(notdir $$@) -y
	$$(touch_target)

$(TMP_DIR)/bs_initialized: $(TMP_DIR)/bs_created ./$(TMP)/deps/ocaml/done
	$$(set_switch) && opam install -y ./$(TMP)/deps/ocaml/_/repo/ocaml-variants.4.06.1+BS.opam
	$$(touch_target)

$(TMP_DIR)/bs_created:
	rm -rf $(TMP_DIR)/switch_bs
	opam switch create $(TMP_DIR)/switch_bs --empty
	$$(touch_target)
endef

$(eval $(cmd))

override $(CLEAN):
	true 'no clean for $@'
