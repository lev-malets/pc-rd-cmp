
DIR=tmp/t/src/parser/test_memo/_

mkdir -p $DIR/$1
dune exec src/parser/test_memo/main.exe -- --input $1 --output $DIR/$1/table

code $DIR/$1/table
