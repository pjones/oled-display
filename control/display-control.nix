{ mkDerivation, aeson, async, base, bytestring, dbus, directory
, filepath, lens, monad-stm, mtl, network, serialport
, servant-server, stdenv, stm, text, time, warp
}:
mkDerivation {
  pname = "display-control";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring dbus directory filepath lens monad-stm
    mtl network serialport servant-server stm text time warp
  ];
  executableHaskellDepends = [
    aeson async base bytestring dbus directory filepath lens monad-stm
    mtl network serialport servant-server stm text time warp
  ];
  license = stdenv.lib.licenses.bsd2;
}
