# 1..n - files

# sloc $1 |
#    sed 's/ :/:/' |
#    sed 's/\s*//' |
#    awk -F ':' '
#        BEGIN {line="file"}
#        NR > 3 && NR < 13 {line = line "," "\"" $1 "\""}
#        END {print line}
#    '
sum=0

for f in $@; do
    #     sloc $f |
    #         sed 's/:\s*/:/' |
    #         awk -F ':' '
    #             BEGIN {line='\"$f\"'}
    #             NR > 3 && NR < 13 {line = line "," $2}
    #             END {print line}
    #         '
    sum=$(expr $sum + $(sloc $f | awk -F ':' 'NR == 5 {print $2}'))
done

echo $sum

#echo sloc tmp/t/deps/syntax/src/res_token.ml
#sloc tmp/t/deps/syntax/src/res_token.ml

#echo sloc src/parser
#sloc src/parser --exclude "test_memo/*|test/*|bench/*|utils/*"
