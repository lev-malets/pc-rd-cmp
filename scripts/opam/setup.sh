set -eu

opam switch create pc-rd-cmp ocaml-variants.4.14.0+options ocaml-option-flambda
eval $(opam env --switch=pc-rd-cmp)

opam install -y \
    dune.3.4.1 \
    yojson.2.0.2 \
    bigstringaf.0.9.0 \
    fix.20220121 \
    cmdliner.1.1.1 \
    core.v0.15.0 \
    core_unix.v0.15.0 \
    alcotest.1.6.0 \
    ppx_deriving.5.2.1 \
    ocamlformat.0.24.1
