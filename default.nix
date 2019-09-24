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
  staticPackage = pkg: pkg.overrideAttrs (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ] ++
    old.configureFlags;
  });
  config = {
    packageOverrides = pkgs: rec {
      grpc = pkgs.callPackage ./nix/grpc.nix { };

      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          ghc =
            super.ghc // { withPackages = if withHoogle then super.ghc.withHoogle else super.ghc ; };

          ghcWithPackages =
            self.ghc.withPackages;

          # We skip tests for most packages to reduce build times.

          range-set-list =
            pkgs.haskell.lib.dontCheck
              (self.callHackage "range-set-list" "0.1.3" { });

          primitive-extras =
            pkgs.haskell.lib.dontCheck
              (self.callHackage "primitive-extras" "0.7.1.1" { });

          # We intentionally use 1.2.0.3 instead of 1.2.0.4 because 1.2.0.4
          # would cause us to recompile the world due to the old version of
          # 'primitive' shipped by nixpkgs.
          stm-hamt =
            pkgs.haskell.lib.dontCheck
              (self.callHackage "stm-hamt" "1.2.0.3" { });

          stm-containers =
            pkgs.haskell.lib.dontCheck
              (self.callHackage "stm-containers" "1.1.0.4" { });

          proto3-wire =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/proto3-wire.nix { });

          proto3-suite =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/proto3-suite.nix { });

          grpc-haskell-core =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/grpc-haskell-core.nix { });

          # Skip tests for grpc-haskell because they depend on the library
          # already being built. This is a known grpc-haskell issue.
          grpc-haskell =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/grpc-haskell.nix { });

          fencer =
            self.callCabal2nix "fencer" (./.) { };
        };
      };
    };
  };
  pkgs =
    if static
    then (import nixpkgs { inherit config; }).pkgsMusl
    else import nixpkgs { inherit config; };
  drv =
    if static
    then staticPackage pkgs.haskellPackages.fencer
    else pkgs.haskellPackages.fencer;
in
  if pkgs.lib.inNixShell
    then
      drv.env.overrideAttrs(attrs:
        { buildInputs =
          [
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.cabal2nix
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.hlint
          ] ++
          [
            pkgs.haskellPackages.zlib
          ] ++
          [
            pkgs.wget
          ] ++
          attrs.buildInputs;
        })
    else
      # https://github.com/Gabriel439/haskell-nix/blob/master/project3/README.md#minimizing-the-closure
      pkgs.haskell.lib.justStaticExecutables drv
