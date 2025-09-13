{
  self,
  lib,
  mkShellNoCC,
  system,
  haskellPackages,
  haskell-language-server,
  fourmolu,
  cabal2nix,
  taplo,
  jq,
}:
mkShellNoCC {
  inputsFrom = map (lib.getAttr "env") [self.packages.${system}.default];
  packages = [
    haskell-language-server
    fourmolu
    cabal2nix
    haskellPackages.cabal-fmt
    haskellPackages.cabal-install
    taplo
    jq
  ];
}
