{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/2128d0aa28edef51fd8fef38b132ffc0155595df.tar.gz") {} }:

let
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_06.overrideScope' (self: super: {
        ocaml = super.ocaml.override { flambdaSupport = true; };
    });
in


pkgs.mkShell {
    nativeBuildInputs =
        (with pkgs; [ git perf-tools linuxPackages.perf ]) ++
        (with ocamlPackages; [ ocaml dune_2 ocaml-lsp core_bench alcotest findlib re ]);
    buildInputs = with ocamlPackages; [ ocamlgraph yojson bigstringaf fix async lwt ocaml-syntax-shims ];
}
