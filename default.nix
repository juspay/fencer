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

  grpc-haskell-source = (import nixpkgs { }).fetchFromGitHub {
    owner = "awakesecurity";
    repo = "gRPC-haskell";
    rev = "41646238443a34f7c4856c89327e7d304e4ffe8b";
    sha256 = "1wz69gvivs4mb4480x5ry9ac8m1hw1kvqz9ir20prnw38r3pdr9f";
  };

  config = {
    packageOverrides = pkgs: rec {

      protobuf = pkgs.protobuf.overrideAttrs (oldAttrs: rec {
        # We want to make sure the result does not drag the C++ compiler
        # into the closure
        disallowedReferences = with pkgs; [ stdenv.cc ];
        # '-g' adds debug symbols to binaries, and those contain paths to
        # the C++ compiler, which get interpreted by Nix as a runtime
        # dependency. See <https://github.com/NixOS/nixpkgs/issues/73919>
        postConfigure = ''
          sed -i -e 's/ -g / /g' Makefile src/Makefile gmock/make/Makefile googletest/make/Makefile
        '';
      });

      grpcUpstream = with pkgs; import "${grpc-haskell-source}/nix/grpc.nix" {
        inherit stdenv fetchFromGitHub cmake zlib c-ares pkgconfig openssl gflags protobuf;
      };
      grpc = grpcUpstream.overrideAttrs (oldAttrs: rec {
        # We want to make sure the result does not drag the C++ compiler
        # into the closure
        disallowedReferences = with pkgs; [ stdenv.cc ];
      });

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

          gpr = grpc;

          grpc-haskell-core =
            pkgs.haskell.lib.dontCheck
              (self.callCabal2nix "grpc-haskell-core"
                 "${grpc-haskell-source}/core" { });

          grpc-haskell =
            pkgs.haskell.lib.dontCheck
              (self.callCabal2nix "grpc-haskell"
                 grpc-haskell-source { });

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
  fencerPre =
    if pkgs.lib.inNixShell
    then drv
    else pkgs.haskell.lib.justStaticExecutables drv;
  drv =
    if static
    then staticPackage pkgs.haskellPackages.fencer
    else pkgs.haskellPackages.fencer;
in {
  pkgs = pkgs;
  fencer =
      if builtins.getEnv "TRAVIS" == "true"
      then pkgs.haskell.lib.dontHaddock fencerPre
      else fencerPre;
}
