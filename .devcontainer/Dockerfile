FROM fedora:34

RUN useradd -ms /bin/bash user
RUN dnf install -y git opam findutils which diffutils gcc-c++ gmp-devel npm
RUN npm install -g sloc@0.2.1

USER user

RUN opam init --shell=bash --shell-setup --disable-sandboxing --bare
RUN opam switch create 4.06.1+rescript
RUN eval $(opam env --switch=4.06.1+rescript --set-switch) && opam install -y \
    ocaml-lsp-server dune.2.9.0 \
    core_bench.v0.11.0 alcotest.1.4.0 \
    yojson.1.7.0 bigstringaf.0.8.0 async.v0.11.0 lwt.5.4.2 \
    fix.20201120

USER root
RUN dnf install -y perf
