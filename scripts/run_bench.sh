set -euo pipefail

D=$(realpath $(dirname $0))
T=$(realpath $1)
CMD=$(realpath $D/..)

make -C $CMD deps

bash $D/bench_cb.sh $T test 1 $T/_cb RedBlackTree:$CMD/tmp/t/deps/syntax/benchmarks/data/RedBlackTree.res
bash $D/bench_perf.sh $T exec 1 $T/_perf RedBlackTree:$CMD/tmp/t/deps/syntax/benchmarks/data/RedBlackTree.res
