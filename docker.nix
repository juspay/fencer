let
  # TODO: this rebuilds 'fencer', can we avoid that?
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
