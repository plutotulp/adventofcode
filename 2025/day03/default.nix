{ mkDerivation, base, lib }:
mkDerivation {
  pname = "day03";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "day03";
}
