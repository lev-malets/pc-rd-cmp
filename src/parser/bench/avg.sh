# 1..n - files

cat $@ | awk '
        BEGIN { cnt = 0; t = 0; m = 0; mj = 0; it = 0; im = 0; img = 0 }
        { cnt += 1; t += $1; m += $2; mj += $3; it += $4; im += $5; img += $6 }
        END { print (t / cnt) " " (m / cnt) " " (mj / cnt) " " (it / cnt) " " (im / cnt) " " (img / cnt) }
    '
