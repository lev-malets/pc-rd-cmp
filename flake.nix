{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/2128d0aa28edef51fd8fef38b132ffc0155595df";
  };

  outputs = { self, ... }@inputs:
    let
      supportedSystems = [ "x86_64-linux" ];
      forEachSystem = inputs.nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = forEachSystem (system: import inputs.nixpkgs
        { inherit system; });

      basic = forEachSystem (system: import ./nix/basic.nix { pkgs = pkgsFor.${system}; });
    in
    {
      packages = forEachSystem
        (system:
          let pkgs = pkgsFor.${system}; in
          rec {
            test-image = import ./test/image/default.nix
              {
                inherit pkgs;
                basic = basic.${system};
              };
            test-image-name = import ./test/image/name.nix {
              inherit pkgs;
              image = test-image;
            };
          }
        );

      devShells = forEachSystem
        (system: with pkgsFor.${system};
        let b = basic.${system}; in
        {
          default = mkShell {
            buildInputs =
              b.buildOnlyDeps ++
              b.buildDeps ++
              b.checkDeps ++
              b.perfDeps ++
              [
                (pkgs.python3.withPackages (ps: with ps; [
                  pandas
                  black
                  ipykernel
                  pip
                  plotly
                  nbformat
                ]))
              ] ++
              [ git ncurses gdb valgrind act ] ++
              (with b.ocamlPackages; [ ocaml-lsp utop ]);
          };

          bench = mkShell {
            buildInputs =
              b.buildOnlyDeps ++
              b.buildDeps ++
              b.perfDeps;
          };
        });
    };
}
