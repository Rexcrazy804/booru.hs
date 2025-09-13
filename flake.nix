{
  description = "A minimal Flake template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    self,
    nixpkgs,
    systems,
    ...
  } @ inputs: let
    inherit (nixpkgs) lib;
    inherit (lib) getAttrs mapAttrs;

    pkgsFor = getAttrs (import systems) nixpkgs.legacyPackages;
    eachSystem = fn: mapAttrs fn pkgsFor;
  in {
    formatter = eachSystem (_: pkgs: pkgs.alejandra);
    devShells = eachSystem (_: pkgs: {default = pkgs.callPackage ./nix/shell.nix {};});
  };
}
