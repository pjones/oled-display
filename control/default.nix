{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghcide ? sources.ghcide-nix
, ormolu ? sources.ormolu
, ghc ? "default"
}:

nix-hs {
  cabal = ./display-control.cabal;
  compiler = ghc;

  overrides = lib: self: super: with lib; {
    ghcide = import ghcide {};

    ormolu = (import ormolu {
      inherit (lib) pkgs;
      ormoluCompiler = lib.compilerName;
    }).ormolu;
  };
}
