# shell.nix

(import ./.).shellFor {
  packages = p: [ p.bimaps p.DPutils p.PrimitiveArray ];
  withHoogle = true;
}

