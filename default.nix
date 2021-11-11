{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.pkgs.haskellPackages.callPackage ./sucodu.nix { }
