{ mkDerivation, base, lib, megaparsec, text }:
mkDerivation {
  pname = "day05";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec text ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "day05";
}
