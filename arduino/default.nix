{ pkgs ? import <nixpkgs> { }
}:

pkgs.stdenv.mkDerivation {
  name = "display";
  src = ./.;

  buildInputs = with pkgs; [
    platformio
  ];
}
