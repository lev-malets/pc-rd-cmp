RES=$1
EXE=$2

set -e

files3=$(find tmp/t/deps/syntax/benchmarks/data -name "*.res" -o -name "*.resi")

rm -rf $RES
mkdir -p $(dirname $RES)

function run {
    sudo nice -n -20 $EXE $@
}

for file in $files1 $files2 $files3; do
    echo $file
    echo '#' $file >>$RES

    echo '##' init dummy >>$RES
    run \
        --parser dummy \
        --input $file \
        --last-stage parser-init |
        tail -n+2 >>$RES
    echo '##' dummy >>$RES
    run \
        --parser dummy \
        --input $file \
        --last-stage parse |
        tail -n+2 >>$RES

    echo '##' rescript >>$RES
    run \
        --parser rescript \
        --input $file \
        --last-stage parse |
        tail -n+2 >>$RES

    for parser in angstrom tokenized; do
        for config in data/configs/memo+peek.json; do
            echo '##' init $parser $config >>$RES
            run \
                --config $config \
                --parser $parser \
                --input $file \
                --last-stage parser-init |
                tail -n+2 >>$RES
            echo '##' $parser $config >>$RES
            run \
                --config $config \
                --parser $parser \
                --input $file \
                --last-stage parse |
                tail -n+2 >>$RES
        done
    done
done
