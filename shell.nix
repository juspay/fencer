let
  drv = import ./default.nix {
    withHoogle = if builtins.getEnv "TRAVIS" != "" then false else true;
  };
  pkgs = drv.pkgs;
in drv.fencer.env.overrideAttrs (attrs: {
  buildInputs = [
    pkgs.haskellPackages.cabal-install
    pkgs.haskellPackages.cabal2nix
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.hlint
  ] ++ [
    pkgs.haskellPackages.zlib
  ] ++ [
    pkgs.wget
  ] ++ attrs.buildInputs;
})
