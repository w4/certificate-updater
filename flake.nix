{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.default = (import ./.) {
          inherit pkgs;
        };

        nixosModules.default = { config, lib, pkgs, ... }:
          (import ./module.nix) {
            inherit config;
            inherit lib;
            pkg = self.packages."${system}".default;
          };
      });
}
