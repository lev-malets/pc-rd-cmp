# 1 - parser; 2 - input

NAME=src/parser/bench/exec_$1.exe
dune build $NAME 2> /dev/null

time _build/default/$NAME --input $2
