{ nixpkgs ? import ./nixpkgs.nix
}:
let
  inherit (nixpkgs) pkgs;

  revealjs = pkgs.fetchgit {
    url = "https://github.com/hakimel/reveal.js.git";
    rev = "3.8.0";
    sha256 = "14cva2hxdv4gxpz2a996qs8xhxffw97a90gkz2mmgdczh1kyn1sc";
  };

  local = ./.;
in
  pkgs.stdenv.mkDerivation {
    name = "ghc-language-extensions-talk";
    src = ./.;
    preferLocalBuild = true;
    allowSubstitutes = false;

    unpackPhase = ''
      mkdir -p $name/{reveal.js,css,images,js}
      cd $name
      cp -r ${revealjs}/* ./reveal.js/
      cp $src/css/* ./css/
      # rm ./css/grid-light.css
      cp $src/images/* ./images/
    '';

    buildPhase = ''
      cat $src/slides/title.md \
          $src/slides/intro.md \
          $src/slides/what-are-extensions.md \
          $src/slides/references.md \
          > slides.md
      pandoc -i -t revealjs --slide-level=2 --template=$src/template.revealjs --variable=codedir:$out --variable=transition:none --no-highlight -s slides.md -o index.html
      rm slides.md
    '';

    installPhase = ''
      mkdir $out
      cp -r ./* $out/
    '';

    phases = ["unpackPhase" "buildPhase" "installPhase"];

    buildInputs = [pkgs.pandoc];
  }
