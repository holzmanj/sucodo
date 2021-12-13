{ pkgs ? import <nixpkgs> {} }:
pkgs.haskellPackages.callPackage ./sucodu.nix { }
