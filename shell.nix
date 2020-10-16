with (import ./default.nix {});

hsPkgs.shellFor {
  packages = _: [
    duckling
  ];

  buildInputs = with pkgs; [
    cabal-install
    ghcid
    stack
  ];
}
