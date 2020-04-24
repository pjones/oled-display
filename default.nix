{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, ghc ? "default"
}:

let
  control = import ./control {
    inherit sources pkgs ghc;
  };

in control.bin
