{ pkgs, basic, ... }:

let
  deps = basic.buildOnlyDeps
    ++ basic.buildDeps
    ++ basic.checkDeps;
  user = "user";
  uid = 1000;
  gid = 100;
  envVarsSetup = with pkgs; [
    (
      runCommand "env-vars-OCAMLPATH" { buildInputs = deps; } ''
        mkdir -p $out/envars
        echo $OCAMLPATH > $out/envars/OCAMLPATH
      ''
    )
  ];
in

pkgs.dockerTools.buildLayeredImage {
  name = "seriousbasket/pc-rd-cmp-test";
  contents =
    (with pkgs; [
      bashInteractive
      bintools-unwrapped
      cacert
      coreutils
      findutils
      gawk
      gcc
      git
      gnumake
      gnused
      gnupatch
    ])
    ++ deps
    ++ envVarsSetup;
  config = {
    Cmd =
      let script = ''
        export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
        export OCAMLPATH=$(cat /envars/OCAMLPATH)
        make test
      ''; in
      [ "bash" "-c" script ];
    WorkingDir = "/repo";
    Volumes = { "/repo" = { }; "/tmp" = { }; };
  };
}
