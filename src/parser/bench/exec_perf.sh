RES=$1
EXE=$2

set -e

files3=$(find tmp/t/deps/syntax/benchmarks/data -name "*.res" -o -name "*.resi")

rm -rf $RES
mkdir -p $(dirname $RES)

function run {
    for x in $(seq 1 100); do
        sudo nice -n -20 perf stat -d $EXE $@ >>$RES 2>&1
    done
}

for file in $files1 $files2 $files3; do
    echo $file
    echo '#' $file >>$RES

    echo '##' init dummy >>$RES
    run \
        --parser dummy \
        --input $file \
        --last-stage parser-init
    echo '##' dummy >>$RES
    run \
        --parser dummy \
        --input $file \
        --last-stage parse

    echo '##' rescript >>$RES
    run \
        --parser rescript \
        --input $file \
        --last-stage parse

    for parser in angstrom tokenized; do
        for config in data/configs/memo+peek.json; do
            echo '##' init $parser $config >>$RES
            run \
                --config $config \
                --parser $parser \
                --input $file \
                --last-stage parser-init
            echo '##' $parser $config >>$RES
            run \
                --config $config \
                --parser $parser \
                --input $file \
                --last-stage parse
        done
    done
done
