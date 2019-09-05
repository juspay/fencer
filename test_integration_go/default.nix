let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "db858b4d3032aec35be7e98a65eb9b91b63671ef";
    sha256 = "0gqcbf5nyqff1a4ps6szcrv59ay97fr26jdwrs7qp8fijzcpdnkh";
  };
  pkgs = import nixpkgs { };

in
pkgs.buildGoPackage rec {
  name = "test_integration_go";
  src = ./.;
  goPackagePath = "test_integration_go";
  goDeps = ./deps.nix;
}
