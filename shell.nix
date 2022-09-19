with import <nixpkgs> {};

let
  myGhc = ghc.withPackages(p : with p; [doctest deepseq ghc-heap-view QuickCheck]);
in stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [ myGhc ghcid ];
  shellHook = ''
    export NIX_GHC=${myGhc}/bin/ghc
    export NIX_GHC_LIBDIR=${myGhc}/lib/ghc-${myGhc.version}
  '';
}
