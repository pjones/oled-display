{ pkgs ? import <nixpkgs> {}
}:

let
  control = import ./control { inherit pkgs; };

in

pkgs.haskell.lib.justStaticExecutables control
