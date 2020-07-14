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
    relude =
      if super ? relude_0_6_0_0
      then super.relude_0_6_0_0
      else super.relude;
  };
}
