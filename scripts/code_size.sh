set -euo pipefail

D=$(realpath $(dirname $0))
T=$(realpath $1)
SYNTAX=$(realpath $2)
CMP=$(realpath $D/..)

xpath=$T/code_size
mkdir -p $xpath

syntax_files=(
	$SYNTAX/src/res_core.ml
	$SYNTAX/src/res_token.ml
)

syntax_hlp_files=(
	$SYNTAX/src/res_parser.ml
)

syntax_all=(
    ${syntax_files[*]}
    ${syntax_hlp_files[*]}
)

pc_files=(
	$CMP/src/parser/basic.ml
    $CMP/src/parser/diagnostics.ml
    $CMP/src/parser/parser_basic.ml
    $CMP/src/parser/parser_expression.ml
    $CMP/src/parser/parser_modexpr.ml
    $CMP/src/parser/parser_modtype.ml
    $CMP/src/parser/parser_pattern.ml
    $CMP/src/parser/parser_type.ml
    $CMP/src/parser/parser.ml
    $CMP/src/parser/parsetree_mapping.ml
    # $CMP/src/parser/sigs.ml

    # $CMP/src/parser/parser_tokenized/lexer.mll
    $CMP/src/parser/parser_tokenized/parser_tokenized.ml
    $CMP/src/parser/parser_tokenized/token.ml
)

pc_hlp_files=(
	$CMP/src/pc/pc.ml
	# $CMP/src/pc/sigs.ml
    $CMP/src/tokenized/make.ml
    $CMP/src/tokenized/parser.ml
    # $CMP/src/tokenized/sigs.ml
    $CMP/src/tokenized/tokenized.ml
    $CMP/src/tokenized/tset.ml
)

pc_all=(
    ${pc_files[*]}
    ${pc_hlp_files[*]}
)

syntax_sha256=$(sha256sum $syntax_all |
    cut -d' ' -f1 | sha256sum | cut -d' ' -f1)
pc_sha256=$(sha256sum $pc_all |
    cut -d' ' -f1 | sha256sum | cut -d' ' -f1)
sha256=$(echo $syntax_sha256 $pc_sha256 | sha256sum | cut -d' ' -f1)

if [[ -f $xpath/.done && $(cat $xpath/.done) = $sha256 ]]; then
    exit 0
fi

function sloc_ {
    sum=0

    for f in $@; do
        sum=$(expr $sum + $(sloc $f | awk -F ':' 'NR == 5 {print $2}'))
    done

    echo $sum
}

rm -rf $xpath/.hlp
mkdir -p $xpath/.hlp

for file in ${syntax_all[*]} ${pc_all[*]}; do
    mkdir -p $xpath/.hlp/$(dirname $file)
    cp $file $xpath/.hlp/$file
done

echo '
profile = default
version = 0.19.0

margin = 80
' > $xpath/.hlp/.ocamlformat

(
    cd $xpath/.hlp
    ocamlformat --inplace $(find . -type f ! -name .ocamlformat ! -name '*.mll')
)

rm -rf $xpath/.out
mkdir -p $xpath/.out

sloc_ ${syntax_files[*]} > $xpath/.out/syntax
sloc_ ${syntax_hlp_files[*]} > $xpath/.out/syntax_hlp

sloc_ ${pc_files[*]} > $xpath/.out/pc
sloc_ ${pc_hlp_files[*]} > $xpath/.out/pc_hlp
