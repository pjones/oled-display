{ pkgs ? import <nixpkgs> { }
}:

pkgs.stdenv.mkDerivation {
  name = "display";
  src = ./.;

  buildInputs = with pkgs; [
    platformio
  ];

  # FIXME: This doesn't work because PlatformIO needs a network
  # connection to download and install atmelvar.
  buildPhase = ''
    pio run
  '';

  installPhase = ''
    mkdir $out
    cp .pioenvs/micro/firmware.hex $out/
  '';

  shellHook = ''
    upload() {
      pio run -t upload
    }

    connect() {
      ${pkgs.picocom}/bin/picocom --echo /dev/ttyACM0
    }
  '';
}
