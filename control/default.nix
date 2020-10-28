{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
, static ? false
}:

nix-hs {
  cabal = ./display-control.cabal;
  compiler = ghc;
  enableFullyStaticExecutables = static;

  overrides = lib: self: super: {
    optparse-applicative =
      if super ? optparse-applicative_0_16_0_0
      then super.optparse-applicative_0_16_0_0
      else super.optparse-applicative;
  };
}
