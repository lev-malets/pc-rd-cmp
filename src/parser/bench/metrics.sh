# 1 - file

cat $1 |
    sed 's/^\xE2\x94\x82\s*//' |
    sed 's/_//g' |
    sed 's/\s*\xE2\x94\x82\s*/_/g' |
    sed 's/\([0-9]*\.[_0-9]*\)/\1_/g' |
    head -n 7 | tail -n 3 |
    awk -F_ '
        NR == 1 { t = $2; m = $4; mj = $6 }
        NR == 2 { t1 = $2; m1 = $4; mj1 = $6 }
        NR == 3 { print (t1 / t) " " (m1 / m) " " (mj1 / mj) " " ($2 / t1) " " ($4 / m1) " " ($6 / mj1) }
    '
