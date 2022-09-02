FROM fedora:36

RUN sudo dnf install -y opam shfmt
RUN dnf install -y git g++ make
RUN useradd -u 1000 user

USER user

RUN opam init --disable-sandboxing --disable-shell-hook
RUN opam switch create pc-rd-cmp ocaml-variants.4.14.0+options ocaml-option-flambda
RUN eval $(opam env --switch=pc-rd-cmp) && opam install -y \
    yojson.2.0.2 \
    bigstringaf.0.9.0 \
    fix.20220121 \
    cmdliner.1.1.1 \
    core.v0.15.0 \
    core_unix.v0.15.0 \
    alcotest.1.6.0 \
    ppx_deriving.5.2.1
RUN eval $(opam env --switch=pc-rd-cmp) && opam install -y \
    dune.3.4.1 \
    ocamlformat.0.24.1
