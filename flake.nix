{
  description = "A minimal Flake template";

  nixConfig.allow-import-from-derivation = true;

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    self,
    nixpkgs,
    systems,
  }: let
    inherit (nixpkgs) lib;
    inherit (lib) getAttrs mapAttrs;

    pkgsFor = getAttrs (import systems) nixpkgs.legacyPackages;
    eachSystem = fn: mapAttrs fn pkgsFor;
  in {
    formatter = eachSystem (_: pkgs: pkgs.alejandra);
    packages = eachSystem (system: pkgs: {
      noifd = pkgs.haskellPackages.callPackage ./nix/package.nix {};
      rapid = pkgs.haskellPackages.callCabal2nix "booru-hs" (self.packages.${system}.noifd.src) {};
      default = self.packages.${system}.noifd;
    });
    devShells = eachSystem (_: pkgs: {
      default = pkgs.callPackage ./nix/shell.nix {inherit self;};
      ci = pkgs.callPackage ./nix/cishell.nix {inherit self;};
    });
  };
}
