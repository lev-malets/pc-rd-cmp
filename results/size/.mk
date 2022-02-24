$K/done: $T/ratio $T/ratio_all

$T/syntax: $(KEYS)/deps/patch/syntax $(syntax_files) $D/code_size.sh
	. $D/code_size.sh $(syntax_files) > $@

$T/syntax_hlp: $(KEYS)/deps/patch/syntax $(syntax_hlp_files) $D/code_size.sh
	. $D/code_size.sh $(syntax_hlp_files) > $@

$T/syntax_all: $T/syntax $T/syntax_hlp
	expr $$(cat $T/syntax) + $$(cat $T/syntax_hlp) > $@

$T/pc: $(pc_files) $D/code_size.sh
	. $D/code_size.sh $(pc_files) > $@

$T/pc_hlp: $(pc_hlp_files) $D/code_size.sh
	. $D/code_size.sh $(pc_hlp_files) > $@

$T/pc_all: $T/pc $T/pc_hlp
	expr $$(cat $T/pc) + $$(cat $T/pc_hlp) > $@

$T/ratio_all: $T/syntax_all $T/pc_all
	expr $$(cat $T/pc_all) '*' 100 '/' $$(cat $T/syntax_all) > $@

$T/ratio: $T/syntax $T/pc
	expr $$(cat $T/pc) '*' 100 '/' $$(cat $T/syntax) > $@

syntax_files := \
	$(call T_of,deps)/syntax/src/res_core.ml \
	$(call T_of,deps)/syntax/src/res_token.ml

syntax_hlp_files := \
	$(call T_of,deps)/syntax/src/res_parser.ml

pc_files := \
	src/parser/basic.ml \
	src/parser/parser_constant.ml \
	src/parser/parser_expression.ml \
	src/parser/parser_modexpr.ml \
	src/parser/parser_modtype.ml \
	src/parser/parser_pattern.ml \
	src/parser/parser_type.ml \
	src/parser/parser_utils.ml \
	src/parser/parser.ml \
	src/parser/parsetree_mapping.ml \
	src/parser/sigs.ml \
	src/parser/state.ml

pc_hlp_files := \
	src/angstrom-mod/lib/angstrom.ml \
	src/angstrom-mod/lib/exported_state.ml \
	src/angstrom-mod/lib/input.ml \
	src/angstrom-mod/lib/input.mli \
	src/angstrom-mod/lib/more.ml \
	src/angstrom-mod/lib/parser.ml \
	src/angstrom-mod/lib/state.ml \
	src/angstrom-pos/angstrom_pos.ml \
	src/angstrom-pos/charset.ml \
	src/angstrom-pos/make.ml \
	src/angstrom-pos/sigs.ml \
	src/angstrom-pos/state.ml
