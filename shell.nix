{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.mkShell {
  inputsFrom = [ (pkgs.haskellPackages.callCabal2nix "certificate-updater" ./. { }).env ];
}
