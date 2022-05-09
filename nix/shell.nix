let p = import ./pkgs.nix; in
let pkgs = p.pkgs; in
let b = import ./basic.nix p; in

pkgs.mkShell {
  nativeBuildInputs =
    (with pkgs; [ git perf-tools linuxPackages.perf ncurses ocamlformat gdb valgrind act ]) ++
    (with b.ocamlPackages; [ ocaml-lsp ]) ++
    b.tools;
  buildInputs = b.libs;
}
