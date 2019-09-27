let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "db858b4d3032aec35be7e98a65eb9b91b63671ef";
    sha256 = "0gqcbf5nyqff1a4ps6szcrv59ay97fr26jdwrs7qp8fijzcpdnkh";
  };
  pkgs = import nixpkgs { };
  # TODO: this rebuilds 'fencer', can we avoid that?
  fencer = import ./default.nix { };

in
pkgs.dockerTools.buildImage {
  name = "juspay/fencer";
  tag = "latest";
  contents = fencer;
  config.Cmd = [ "${fencer}/bin/fencer" ];
}
