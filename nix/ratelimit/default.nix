{ pkgs ? import (import ./../../nixpkgs.nix) {} }:

pkgs.buildGoPackage rec {
  name = "ratelimit-${version}";
  version = "2019-10-29";
  # NOTE: When updating the rev, you must regenerate deps.nix (see README)
  rev = "8d923502fb14fa2b2d31c945676dcea503f11e0b";

  goPackagePath = "github.com/lyft/ratelimit";

  src = pkgs.fetchgit {
    inherit rev;
    url = "https://github.com/lyft/ratelimit.git";
    sha256 = "0rpky5vlfixnxy22rr2x6ky371xgqnkfp8s2pvpcw1f3cjjmz09v";
  };

  goDeps = ./deps.nix;

  meta = {
    description = "Go/gRPC service for rate limiting";
    longDescription = "Go/gRPC service designed to enable generic rate limit scenarios from different types of applications.";
    homepage = "https://github.com/lyft/ratelimit";
  };
}
