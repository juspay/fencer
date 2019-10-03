{ withHoogle ? false
, static ? false
}:
let
  nixpkgs = import ./nixpkgs.nix;
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
            if withHoogle
            then super.ghc // {withPackages = super.ghc.withHoogle;}
            else super.ghc;

          ghcWithPackages =
            if withHoogle
            then self.ghc.withPackages
            else super.ghcWithPackages;

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
in {
  pkgs = pkgs;
  fencer =
    if pkgs.lib.inNixShell
    then drv
    else pkgs.haskell.lib.justStaticExecutables drv;
}
