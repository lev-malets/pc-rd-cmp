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

set_switch := eval $$(opam env --switch=$(TMP_DIR)/switch --set-switch)

define cmd
$(DONE): $(DEPS_TARGETS)
	$$(touch_target)

$(DEPS_TARGETS): $(TMP_DIR)/libs/%: $(TMP_DIR)/created
	$$(set_switch) && opam install $$(notdir $$@) -y
	$$(touch_target)

$(TMP_DIR)/created:
	rm -rf $(TMP_DIR)
	opam switch create $(TMP_DIR)/switch 4.06.1+flambda
	$$(touch_target)
endef

$(eval $(cmd))

override $(CLEAN):
	true 'no clean for $@'
