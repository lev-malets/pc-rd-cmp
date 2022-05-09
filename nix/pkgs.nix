rec {
  gitsum = "2128d0aa28edef51fd8fef38b132ffc0155595df";
  source = fetchTarball "https://github.com/NixOS/nixpkgs/archive/${gitsum}.tar.gz";
  pkgs = import source { };
}
