{ pkgs ? import <nixpkgs> { }
}:

pkgs.stdenv.mkDerivation {
  name = "firmware";
  src = ./.;

  buildInputs = with pkgs; [
    arduino-core
    arduino-mk
    pkgsCross.avr.buildPackages.gcc
    pkgsCross.avr.buildPackages.binutils
  ];

  ARDUINO_DIR = "${pkgs.arduino-core}/share/arduino";
  ARDMK_DIR = toString pkgs.arduino-mk;

  installPhase = ''
    mkdir -p $out/firmware
    cp build/arduino.hex $out/firmware/
  '';

  shellHook = ''
    connect() {
      ${pkgs.picocom}/bin/picocom --echo /dev/ttyACM0
    }
  '';
}
