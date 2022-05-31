{ pkgs, image, ... }:

let d = derivation image; in

pkgs.runCommand "test-image-name" { } ''
  echo ${d.imageName}:${d.imageTag} > $out
''
