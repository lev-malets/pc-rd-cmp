
# 1 - file; 2 - flags; 3 - flags

filename=$1
dir=tmp/t/src/parser/test/exec.sh/$1

mkdir -p $dir
dune exec $(dirname $0)/print_actual.exe -- $2 --input $filename --output $dir/actual
if [[ "$?" != "0" ]]; then cat $dir/actual.log; exit 1; fi
dune exec $(dirname $0)/print_expected.exe -- $3 --input $filename --output $dir/expected

if diff $dir/expected $dir/actual > /dev/null; then
    true
else
    code --diff $dir/expected $dir/actual
    exit 1
fi
