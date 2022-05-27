{ pkgs, ... }:

rec {
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_06.overrideScope' (self: super: rec {
    ocaml = super.ocaml.override { flambdaSupport = true; };

    ppx_deriving = super.buildDunePackage rec {
      pname = "ppx_deriving";
      version = "v4.5";

      src = pkgs.fetchFromGitHub {
        owner = "ocaml-ppx";
        repo = pname;
        rev = version;
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

  libs = with ocamlPackages; [
    yojson
    bigstringaf
    fix
    async
    lwt
    ocaml-syntax-shims
    cmdliner
    core_bench
    core_kernel
    alcotest
  ];
  tools =
    (with ocamlPackages; [ ocaml dune_2 findlib ])
    ++
    (with pkgs; [ ocamlformat ]);
}
