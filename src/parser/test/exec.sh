
# 1 - parser; 2 - file

filename=$2
dir=tmp/t/src/parser/test/exec.sh/$2

mkdir -p $dir
dune exec $(dirname $0)/main.exe -- --parser $1 --simplified --input $filename --output $dir/actual 2> $dir/actual.log
if [[ "$?" != "0" ]]; then cat $dir/actual.log; exit 1; fi
dune exec $(dirname $0)/main.exe -- --parser res --simplified --input $filename --output $dir/expected 2> $dir/expected.log

code --diff $dir/expected $dir/actual
exit 1
