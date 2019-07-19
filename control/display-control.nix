{ mkDerivation, async, base, bytestring, serial, stdenv }:
mkDerivation {
  pname = "display-control";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ async base bytestring serial ];
  license = stdenv.lib.licenses.bsd2;
}
