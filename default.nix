{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, ghc ? "default"
}:
let
  control = import ./control {
    inherit sources pkgs ghc;
  };

  firmware = import ./arduino {
    inherit pkgs;
  };

in
pkgs.stdenv.mkDerivation {
  name = "display-control";
  phases = [ "installPhase" "fixupPhase" ];
  installPhase = ''
    mkdir -p $out/bin $out/firmware
    install -m 444 ${firmware}/firmware/arduino.hex $out/firmware
    install -m 555 ${control.bin}/bin/display-control $out/bin
  '';
}
