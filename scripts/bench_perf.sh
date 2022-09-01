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

exe=$T/_build_release/default/src/parser/utils/exec.exe
dune build \
    --root $CMP \
    --build-dir $T/_build_release \
    --profile release \
    --always-show-command-line \
    src/parser/utils/exec.exe

rm -rf $res
mkdir -p $(dirname $res)

csv_columns=(
    task-clock
    cycles
    instructions
    branches
)

function run {
    idx=$1
    file=${files[$idx]}
    res_name=${names[$idx]}
    parser=$2
    last_stage=$3
    config=$4

    nice=$(test $cmd = test && echo || echo sudo nice -n -20)
    output=$($nice $(which perf) stat $exe \
        --input "$file" \
        --parser $parser \
        --last-stage $last_stage \
        --config $config \
        2>/dev/stdout 1>/dev/null)
    output=$(echo "$output" | awk -F# '{print $1}')

    answer="$res_name,$parser,$last_stage"
    for col in $(seq 0 $(expr ${#csv_columns[@]} - 1)); do
        found="$(echo "$output" | grep "${csv_columns[$col]}" | awk '{print $1}' | sed 's/,//g')"
        if [[ -z $found ]]; then
            answer="$answer,"
        else
            answer="$answer,$found"
        fi
    done

    user_time=$(echo "$output" | tail -n 2 | head -n 1 | awk '{print $1}')
    sys_time=$(echo "$output" | tail -n 1 | head -n 1 | awk '{print $1}')

    echo "$answer,$user_time,$sys_time" >>$res

}

function run_many_times {
    for x in $(seq 1 $n); do
        echo perf $x $@
        if [[ $x != 1 ]]; then sleep 1; fi
        run $@
    done
}

header="file,parser,last-stage"
for col in $(seq 0 $(expr ${#csv_columns[@]} - 1)); do
    header="$header,${csv_columns[$col]}"
done
header="$header,user-time,sys-time"

echo $header >>$res

for idx in $(seq 0 $(expr $# - 1)); do
    run_many_times $idx dummy init /dev/null
    run_many_times $idx rescript parse /dev/null

    if [[ $cmd == "test" ]]; then break; fi

    for parser in angstrom tokenized; do
        run_many_times $idx $parser init data/configs/bench.json
        run_many_times $idx $parser parse data/configs/bench.json
    done
done
