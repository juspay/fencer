{
  withHoogle ? true
, static ? false
}:
let
  inherit (import <nixpkgs> {}) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "db858b4d3032aec35be7e98a65eb9b91b63671ef";
    sha256 = "0gqcbf5nyqff1a4ps6szcrv59ay97fr26jdwrs7qp8fijzcpdnkh";
  };
  staticAttrs = {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ];
  };
  config = {
    packageOverrides = pkgs: rec {
      grpc = pkgs.callPackage ./nix/grpc.nix { };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc =
            super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };

          ghcWithPackages =
            self.ghc.withPackages;

          range-set-list =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/range-set-list.nix { });

          proto3-wire =
            self.callPackage ./nix/proto3-wire.nix { };

          proto3-suite =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/proto3-suite.nix { });

          # Skip tests for grpc-haskell because they depend on the library
          # already being built.
          grpc-haskell =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/grpc-haskell.nix { });

          grpc-haskell-core =
            self.callPackage ./nix/grpc-haskell-core.nix { };

          ratelimit =
            self.callPackage ./ratelimit.nix (pkgs.lib.optionalAttrs static staticAttrs);
        };
      };
    };
  };
  pkgs =
    if static
    then (import nixpkgs { inherit config; }).pkgsMusl
    else import nixpkgs { inherit config; };
  drv = pkgs.haskellPackages.ratelimit;
in
  if pkgs.lib.inNixShell
    then
      drv.env.overrideAttrs(attrs:
        { buildInputs =
          with pkgs.haskellPackages;
          [
            cabal-install
            cabal2nix
            ghcid
            hindent
            hlint
            stylish-haskell
          ] ++ [ zlib ] ++ attrs.buildInputs;
        })
        else drv.overrideAttrs(attrs:
        {
          # TODO : Create a FHS using buildFHSUserEnv
          buildInputs = [ pkgs.zlib pkgs.nss pkgs.nssmdns pkgs.iana-etc pkgs.cacert ] ++ attrs.buildInputs;
        })
