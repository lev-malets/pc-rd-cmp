set -euo pipefail

D=$(realpath $(dirname $0))
T=$(realpath $1)
CMP=$(realpath $D/../..)

function fetch_tarball {
    local flatten=${flatten:-true}
    local file_name=${file_name:-$(basename $uri)}
    if [[ -z ${files:-} ]]; then
        local files=""
    else
        local files="${files[*]}"
    fi
    local patch=${patch:-}
    if [[ -z $patch ]]; then
        local patch_sum=0
    else
        local patch_sum=$(sha256sum $patch | cut -d' ' -f1)
    fi
    local sum=$(echo $sha256 ';' $files ';' $patch_sum ';' $flatten | sha256sum | cut -d' ' -f1)

    if [[ ! -f $T/deps/.done.$name || $(cat $T/deps/.done.$name) != $sum ]]; then
        if [[ ! -f $T/deps/.tb.$name/$file_name ||
            $(sha256sum -b $T/deps/.tb.$name/$file_name | cut -d' ' -f1) != $sha256 ]]; then
            rm -rf $T/deps/.tb.$name
            mkdir -p $T/deps/.tb.$name

            curl -L -o $T/deps/.tb.$name/$file_name $uri

            [[ $(sha256sum -b $T/deps/.tb.$name/$file_name | cut -d' ' -f1) == $sha256 ]]
        fi

        rm -rf $T/deps/.hlp.$name
        mkdir -p $T/deps/.hlp.$name
        rm -rf $T/deps/$name
        mkdir -p $T/deps/$name
        (
            cd $T/deps/.hlp.$name
            tar xf $T/deps/.tb.$name/$file_name
            if [[ $flatten == true ]]; then
                cd *
            fi
            if [[ -z $files ]]; then
                mv * $T/deps/$name
            else
                mv $files $T/deps/$name
            fi

            if [[ -n $patch ]]; then
                patch -d $T/deps/$name -p1 <$patch
            fi
        )

        echo $sum >$T/deps/.done.$name
        touch $T/.done.deps
    fi
}

(
    name=angstrom
    uri='https://github.com/inhabitedtype/angstrom/archive/b0e7849ec59746b44753a4c5f5954ba95d1c6c5a.tar.gz'
    sha256=7797b56034ba303509ef78da0c8981cfbe9013532bf6a92aaa7962a617bcd939
    files=(
        lib
        dune-project
        angstrom.opam
    )
    patch=$CMP/deps/angstrom.patch

    fetch_tarball
)

(
    name=syntax
    uri='https://github.com/rescript-lang/syntax/archive/a4a5bdaa602870136f44216e006fbac80304db6a.tar.gz'
    sha256=7fe501f26468c5a0f0a16ef88c8c9c6439e54cec796e13b515820ed3f38bfdb4
    patch=$CMP/deps/syntax.patch

    fetch_tarball
)

(
    name=treefmt
    uri='https://github.com/numtide/treefmt/releases/download/v0.4.1/treefmt-x86_64-unknown-linux-gnu.tar.gz'
    sha256=df216a3d886c86c3fc2ad314159d5ec2f22823e937bdbd5eb782571ffc15a3de
    flatten=false

    fetch_tarball
)
