{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./sucodo.nix { }
