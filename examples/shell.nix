{nixpkgs ? import ./nixpkgs.nix}:

let
  inherit (nixpkgs) pkgs;

  gWithP = pkgs.haskell.packages.ghc864.ghcWithPackages (hp: with hp; [
    checkers
    ]);
in
  pkgs.mkShell {
    buildInputs = [gWithP];
  }
