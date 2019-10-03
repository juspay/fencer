let
  drv = import ./default.nix { };
  pkgs = drv.pkgs;
  fencer = drv.fencer;
in
pkgs.dockerTools.buildImage {
  name = "juspay/fencer";
  tag = "latest";
  contents = fencer;
  config.Cmd = [ "${fencer}/bin/fencer" ];
}
