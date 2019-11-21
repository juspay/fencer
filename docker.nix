let
  drv = import ./default.nix { };
  pkgs = drv.pkgs;
  fencer = drv.fencer;
in
pkgs.dockerTools.buildImage {
  name = "juspayin/fencer";
  tag = "latest";
  created = "now";
  contents = fencer;
  config.Cmd = [ "${fencer}/bin/fencer" ];
}
