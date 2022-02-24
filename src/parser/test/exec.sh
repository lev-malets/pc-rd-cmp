
# 1 - output dir

files=$(
    find data/res -name '*.res' -o -name '*.resi'
    find tmp/t/deps/syntax/tests/parsing/grammar -name '*.res' -o -name '*.resi'
    find tmp/t/deps/syntax/benchmarks/data -name '*.res' -o -name '*.resi'
)

rm -rf $1/fail
dune exec $(dirname $0)/fast.exe -- --simplified --output=$1/fail $files 2> $1/log
if [[ "$?" != "0" ]]; then exit 1; fi
echo $filename
if [[ ! -f $1/fail ]]; then exit 0; fi
filename=$(cat $1/fail)

dune exec $(dirname $0)/main.exe -- --parser pc --simplified --input $filename --output $1/actual 2> $1/actual.log
if [[ "$?" != "0" ]]; then cat $1/actual.log; exit 1; fi
dune exec $(dirname $0)/main.exe -- --parser res --simplified --input $filename --output $1/expected 2> $1/expected.log

code --diff $1/expected $1/actual
exit 1
