{nixpkgs ? import <nixpkgs> {}}:

let
  inherit (nixpkgs) pkgs;
in
  pkgs.mkShell {
    buildInputs = [pkgs.haskell.compiler.ghc864];
  }
