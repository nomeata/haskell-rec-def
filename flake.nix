{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/release-22.11;
  description = "rec-def development environment";
  outputs = { self, nixpkgs }: {
    devShell.x86_64-linux =
     with nixpkgs.legacyPackages.x86_64-linux;
     let
       myGhc = ghc.withPackages(p : with p; [doctest deepseq ghc-heap-view QuickCheck]);
     in mkShell rec {
       name = "env";
       packages = [ myGhc ghcid haskell-ci ];
       shellHook = ''
         export NIX_GHC=${myGhc}/bin/ghc
         export NIX_GHC_LIBDIR=${myGhc}/lib/ghc-${myGhc.version}
       '';
     };
  };
}
