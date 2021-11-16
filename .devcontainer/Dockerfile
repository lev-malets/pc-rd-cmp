FROM fedora:34

RUN useradd -ms /bin/bash user
RUN dnf install -y git opam findutils which diffutils gcc-c++

USER user

RUN opam init --shell=bash --shell-setup --disable-sandboxing --bare
RUN opam switch create 4.06.1+rescript
RUN eval $(opam env --switch=4.06.1+rescript --set-switch) && opam install -y \
    ocaml-lsp-server dune fmt odoc \
    core_bench alcotest ppx_jane ppx_deriving \
    extlib yojson bigstringaf async lwt
RUN eval $(opam env --switch=4.06.1+rescript --set-switch) && opam install -y fix
RUN eval $(opam env --switch=4.06.1+rescript --set-switch) && opam install -y easy_logging
RUN eval $(opam env --switch=4.06.1+rescript --set-switch) && opam install -y containers
