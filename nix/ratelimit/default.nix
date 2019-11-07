{ pkgs ? import (import ./../../nixpkgs.nix) {} }:

let
  # NOTE: When updating the rev, you must regenerate deps.nix (see README)
  rev = "8d923502fb14fa2b2d31c945676dcea503f11e0b";
  src = pkgs.fetchgit {
    inherit rev;
    url = "https://github.com/lyft/ratelimit.git";
    sha256 = "0rpky5vlfixnxy22rr2x6ky371xgqnkfp8s2pvpcw1f3cjjmz09v";
  };

  ratelimit = pkgs.buildGoPackage rec {
    inherit rev src;
    name = "ratelimit-${version}";
    version = "2019-10-29";

    goPackagePath = "github.com/lyft/ratelimit";

    goDeps = ./deps.nix;

    meta = {
      description = "Go/gRPC service for rate limiting";
      longDescription = "Go/gRPC service designed to enable generic rate limit scenarios from different types of applications.";
      homepage = "https://github.com/lyft/ratelimit";
    };
  };

  ratelimit-server-example = pkgs.writeShellScriptBin "ratelimit-server-example" "
set -e
export USE_STATSD=false
export LOG_LEVEL=debug
export REDIS_SOCKET_TYPE=tcp
export REDIS_URL=localhost:6379
export RUNTIME_ROOT=${src}/examples
export RUNTIME_SUBDIRECTORY=ratelimit
${ratelimit}/bin/service_cmd
";

  ratelimit-renamed = pkgs.runCommand "ratelimit-renamed" {
    buildInputs = [ pkgs.grpcurl ratelimit ratelimit-server-example ];
  } ''
    mkdir -p "$out/bin"
    cp ${ratelimit-server-example}/bin/* $out/bin/
    # Rename binaries as ratelimit's Makefile does.
    cp ${ratelimit}/bin/service_cmd $out/bin/ratelimit
    cp ${ratelimit}/bin/client_cmd $out/bin/ratelimit_client
    cp ${ratelimit}/bin/config_check_cmd $out/bin/ratelimit_config_check
    '';

in pkgs.runCommand "dummy" {
  buildInputs = [ pkgs.grpcurl ratelimit-renamed ratelimit-server-example ];
}
  ''
  mkdir -p $out/bin
  cp -r ${ratelimit-renamed}/bin $out/
  ''
