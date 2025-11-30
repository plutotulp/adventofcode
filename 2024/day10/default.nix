{ mkDerivation, base, containers, lib }:
mkDerivation {
  pname = "day10";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "day10";
}
