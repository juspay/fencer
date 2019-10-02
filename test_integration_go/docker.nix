let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "db858b4d3032aec35be7e98a65eb9b91b63671ef";
    sha256 = "0gqcbf5nyqff1a4ps6szcrv59ay97fr26jdwrs7qp8fijzcpdnkh";
  };
  pkgs = import nixpkgs { };
  test_integration_go = import ./default.nix;

in
pkgs.dockerTools.buildImage {
  name = "test_integration_go";
  tag = "latest";
  contents = test_integration_go;
  config.Cmd = [ "${test_integration_go}/bin/test_integration_go" ];
}
