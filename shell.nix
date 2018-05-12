(import ./.).shellFor {
  packages = p: [ p.DPutils p.OrderedBits p.PrimitiveArray ];
  withHoogle = true;
}
