{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    (import ./default.nix { inherit pkgs; })
    pkgs.cabal-install
  ];
}
