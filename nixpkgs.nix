let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/db858b4d3032aec35be7e98a65eb9b91b63671ef";
    sha256 = "0gqcbf5nyqff1a4ps6szcrv59ay97fr26jdwrs7qp8fijzcpdnkh";
  };
in nixpkgs
