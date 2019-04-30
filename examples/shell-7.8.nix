{nixpkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs/archive/83b35508c6491103cd16a796758e07417a28698b.tar.gz){}}:

let
  inherit (nixpkgs) pkgs;
in
  pkgs.mkShell {
    buildInputs = [pkgs.haskell.compiler.ghc763 pkgs.haskellPackages.cabal-install];
  }
