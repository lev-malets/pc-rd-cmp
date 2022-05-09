let p = import ../../nix/pkgs.nix; in
let pkgs = p.pkgs; in
let b = import ../../nix/basic.nix p; in

let
  user = "user";
  uid = 1000;
  gid = 100;
  envVarsSetup = with pkgs; [
    (
      runCommand "env-vars-OCAMLPATH" { buildInputs = b.libs ++ b.tools; } ''
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
    ++ b.libs ++ b.tools
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
