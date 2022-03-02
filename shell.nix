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

        ppx_deriving_yojson = super.buildDunePackage rec {
            pname = "ppx_deriving_yojson";
            version = "3.6.0";

            useDune2 = true;

            src = pkgs.fetchurl {
                url = "https://github.com/ocaml-ppx/ppx_deriving_yojson/releases/download/v${version}/ppx_deriving_yojson-v${version}.tbz";
                sha256 = "sha256:d6f66c6f76b5caa9b2f91ad61a8d8142f4e0582d9c6f39ea42b56491048358bc";
            };

            checkInputs = [ super.ounit ];
            buildInputs = [ ];

            propagatedBuildInputs = [ super.yojson super.ppx_deriving super.ppxlib ];

            doCheck = true;
        };

        jupyter = super.buildDunePackage rec {
            pname = "jupyter";
            version = "2.7.6";

            useDune2 = true;

            src = pkgs.fetchurl {
                url = "https://github.com/akabe/ocaml-jupyter/releases/download/v${version}/jupyter-${version}.tbz";
                sha256 = "sha256:1ce9c25d5282ead24bf47e75605ae0300443b95255e9ff5bdd8173283b4bbc5f";
            };

            nativeBuildInputs = [ super.findlib ];
            buildInputs = [
                ppx_deriving_yojson super.cppo super.merlin
                super.lwt_ppx super.uuidm super.base64 super.logs super.cryptokit
                super.zmq-lwt
            ];

            checkInputs = [ super.ounit2 ];

            doCheck = true;
        };
    });
in


pkgs.mkShell {
    nativeBuildInputs =
        (with pkgs; [ git perf-tools linuxPackages.perf ncurses ]) ++
        (with ocamlPackages; [ utop ocaml dune_2 ocaml-lsp core_bench alcotest findlib re ]);
    buildInputs = with ocamlPackages; [ ocamlgraph yojson bigstringaf fix async lwt ocaml-syntax-shims ];
}
