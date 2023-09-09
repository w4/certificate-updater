{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages.default = pkgs.haskellPackages.mkDerivation {
          pname = "certificate-updater";
          version = "0.1.0.0";
          src = ./.;
          isLibrary = false;
          isExecutable = true;
          executableHaskellDepends = with pkgs.haskellPackages; [
            base optparse-generic rio aeson time http-conduit http-types bytestring unordered-containers unix
          ];
          license = "WTFPL";
        };

        nixosModules.default = { config, lib, pkgs, ... }:
          (import ./module.nix) {
            inherit config;
            inherit lib;
            pkg = self.packages."${system}".default;
          };
      });
}
