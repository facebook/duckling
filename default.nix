{ pkgs ? import ./.nix/nixpkgs.nix {}
, compiler ? import ./.nix/compiler.nix
}:

rec {
  inherit pkgs;

  hsPkgs = pkgs.haskell.packages.${compiler};

  duckling = hsPkgs.callCabal2nix "duckling" ./. {};
}
