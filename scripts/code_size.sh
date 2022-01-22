echo sloc tmp/t/deps/syntax/src/res_core.ml
sloc tmp/t/deps/syntax/src/res_core.ml

echo sloc tmp/t/deps/syntax/src/res_token.ml
sloc tmp/t/deps/syntax/src/res_token.ml

echo sloc src/parser
sloc src/parser --exclude "test_memo/*|test/*|bench/*|utils/*"
