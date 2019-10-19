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
              (self.callCabal2nix "proto3-wire"
                 (pkgs.fetchgit {
                    url = "https://github.com/awakenetworks/proto3-wire.git";
                    sha256 = "16l1rnnygwk1b2sb3l6klhr6ad0wvry204icxnc81c6rbzbk6rqc";
                    rev = "4f355bbac895d577d8a28f567ab4380f042ccc24";
                    fetchSubmodules = true;
                  }) { });

          proto3-suite =
            pkgs.haskell.lib.dontCheck
              (self.callCabal2nix "proto3-suite"
                (pkgs.fetchgit {
                   url = "https://github.com/awakesecurity/proto3-suite.git";
                   sha256 = "0g7j7axx9rkrzw32ky9xl08zj34rx4mqafd89lrpnsi8lcq2z06j";
                   rev = "3f6dd6f612cf2eba3c05798926ff924b0d5ab4fa";
                   fetchSubmodules = true;
                 }) { });

          grpc-haskell-core =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/grpc-haskell-core.nix { });

          # Skip tests for grpc-haskell because they depend on the library
          # already being built. This is a known grpc-haskell issue.
          grpc-haskell =
            pkgs.haskell.lib.dontCheck
              (self.callPackage ./nix/grpc-haskell.nix { });

          fencer =
            self.callCabal2nix "fencer" (pkgs.lib.cleanSource ./.) { };
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
