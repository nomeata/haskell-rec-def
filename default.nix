with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "env";
  buildInputs = [
    (ghc.withPackages(p : with p; [doctest]))
    ghcid
  ];
}
