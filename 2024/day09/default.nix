{ mkDerivation, base, containers, lib, mtl }:
mkDerivation {
  pname = "day09";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers mtl ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "day09";
}
