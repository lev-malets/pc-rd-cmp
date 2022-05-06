{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/2128d0aa28edef51fd8fef38b132ffc0155595df.tar.gz") {} }:

let
    ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_06.overrideScope' (self: super: rec {
        ocaml = super.ocaml.override { flambdaSupport = true; };

        ppx_deriving = super.buildDunePackage rec {
            pname = "ppx_deriving";
            version = "v4.5";

            src = pkgs.fetchFromGitHub {
                owner  = "ocaml-ppx";
                repo   = pname;
                rev    = version;
                sha256 = "1v2xldag54n0xk69vv3j4nln9bzkkpq3rildq118sydzsc9v239z";
            };

            buildInputs = with super; [ ppxfind cppo ];
            propagatedBuildInputs = [
                super.ocaml-migrate-parsetree
                super.ppx_derivers
                super.result
                super.ppx_tools
            ];

            doCheck = true;
            checkInputs = [ super.ounit ];
        };
    });
in


pkgs.mkShell {
    nativeBuildInputs =
        (with pkgs; [ git perf-tools linuxPackages.perf ncurses ocamlformat gdb valgrind ]) ++
        (with ocamlPackages; [ utop ocaml dune_2 ocaml-lsp core_bench alcotest findlib ]);
    buildInputs = with ocamlPackages; [ ocamlgraph yojson bigstringaf fix async lwt ocaml-syntax-shims cmdliner ];
}
