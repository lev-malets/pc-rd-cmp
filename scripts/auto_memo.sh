set -euo pipefail

D=$(realpath $(dirname $0))
T=$(realpath $1)
CMP=$(realpath $D/..)

DIR=tmp/t/src/parser/test_memo/auto/_

options=(
    $(find $CMP/tmp/t/deps/syntax/tests/parsing/grammar -name '*.res' |
        awk '{print "--input=" $1 "*1"}')
)

mkdir -p $T/.auto_memo
dune exec \
    --root $CMP \
    --build-dir $T/_build_release \
    --profile release \
    --always-show-command-line \
    src/parser/utils/auto_memo.exe -- ${options[*]} --output $T/.auto_memo/table

code $T/.auto_memo/table
