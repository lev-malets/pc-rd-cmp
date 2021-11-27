
# 1 - input; 2 - output dir

dune exec $(dirname $0)/fast.exe -- --simplified --input $1 2> $2/log

if [[ "$?" = "0" ]]; then exit 0; fi

set -e
dune exec $(dirname $0)/main.exe -- --parser pc --simplified --input $1 --output $2/actual 2> $2/actual.log
dune exec $(dirname $0)/main.exe -- --parser res --simplified --input $1 --output $2/expected 2> $2/expected.log

code --diff $2/expected $2/actual
exit 1
