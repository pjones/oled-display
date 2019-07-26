{ mkDerivation, async, base, bytestring, dbus, lens, monad-stm, mtl
, serial, stdenv, stm, time
}:
mkDerivation {
  pname = "display-control";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring dbus lens monad-stm mtl serial stm time
  ];
  executableHaskellDepends = [
    async base bytestring dbus lens monad-stm mtl serial stm time
  ];
  license = stdenv.lib.licenses.bsd2;
}
