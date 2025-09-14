{
  self,
  lib,
  mkShellNoCC,
  system,
  haskellPackages,
}:
mkShellNoCC {
  inputsFrom = map (lib.getAttr "env") [self.packages.${system}.rapid];
  packages = [haskellPackages.cabal-install];
}
