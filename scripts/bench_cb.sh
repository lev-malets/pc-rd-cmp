set -euo pipefail

D=$(realpath $(dirname $0))
T=$(realpath $1)
CMP=$(realpath $D/..)

cmd=$2
n=$3
res=$4

shift 4

names=()
files=()

for map in $@; do
    names+=($(cut -d: -f1 <<<$map))
    files+=($(cut -d: -f2 <<<$map))
done

exe=$T/_build_release/default/src/parser/bench/main.exe
dune build \
    --root $CMP \
    --build-dir $T/_build_release \
    --profile release \
    --always-show-command-line \
    src/parser/bench/main.exe

rm -rf $res
mkdir -p $(dirname $res)

csv_columns=(Time/Run mWd/Run mjWd/Run Prom/Run mGC/Run mjGC/Run Comp/Run)

function run {
    idx=$1
    file=${files[$idx]}
    res_name=${names[$idx]}
    parser=$2
    last_stage=$3
    config=$4
    quota=$(test $cmd = test && echo 0.1 || echo 10)

    nice=$(test $cmd = test && echo || echo sudo nice -n -20)
    output=$(
        $nice $exe \
            --input "$file" \
            --parser $parser \
            --last-stage $last_stage \
            --config $config \
            --quota $quota
    )
    out_names=($(echo "$output" | tail -n+3 | head -n1 | sed 's/\xe2\x94\x82/ /g'))
    out_values=($(echo "$output" | tail -n+5 | head -n1 | sed 's/\xe2\x94\x82/ /g'))

    answer="$res_name,$parser,$last_stage"
    for col in $(seq 0 $(expr ${#csv_columns[@]} - 1)); do
        found=""
        for out_i in $(seq 1 $(expr ${#out_names[*]} - 1)); do
            if [[ ${out_names[$out_i]} == ${csv_columns[$col]} ]]; then
                found=$(echo ${out_values[$out_i]} |
                    sed 's/\([0-9.-e]\+\)\(ns\|ms\|us\|w\|kw\|Mw\)/\1 \2/' |
                    sed 's/w//' |
                    sed 's/ns\|kw/1000/' |
                    sed 's/us\|Mw/1000000/' |
                    sed 's/ms/1000000000/' |
                    awk 'NF > 1 {print $1 * $2} NF == 1 {print $1}')
                break
            fi
        done
        if [[ -z $found ]]; then
            answer="$answer,"
        else
            answer="$answer,$found"
        fi
    done

    echo $answer >>$res
}

function run_many_times {
    for x in $(seq 1 $n); do
        echo cb $x $@
        run $@
    done
}

header="file,parser,last-stage"
for col in $(seq 0 $(expr ${#csv_columns[@]} - 1)); do
    header="$header,${csv_columns[$col]}"
done

echo $header >>$res

for idx in $(seq 0 $(expr $# - 1)); do
    run_many_times $idx dummy init /dev/null
    run_many_times $idx rescript parse /dev/null

    if [[ $cmd == "test" ]]; then break; fi

    for parser in angstrom tokenized; do
        for config in data/configs/bench.json; do
            run_many_times $idx $parser init $config
            run_many_times $idx $parser parse $config
        done
    done
done
