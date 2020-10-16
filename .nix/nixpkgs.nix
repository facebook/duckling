{ config ? {}
, overlays ? []
}:

with {
  owner = "NixOS";
  repo = "nixpkgs";
  rev = "5272327b81ed355bbed5659b8d303cf2979b6953";
  sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
};

import (builtins.fetchTarball {
  url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  inherit sha256;
}) { inherit config overlays; }
