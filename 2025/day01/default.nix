{ mkDerivation, base, lib, megaparsec, text }:
mkDerivation {
  pname = "day01";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec text ];
  executableHaskellDepends = [ base text ];
  license = lib.licenses.bsd3;
  mainProgram = "day01";
}
