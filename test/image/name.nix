let image = import ./default.nix; in
let d = derivation image; in

"${d.imageName}:${d.imageTag}"
