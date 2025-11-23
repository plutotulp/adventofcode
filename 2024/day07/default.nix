{ mkDerivation, base, lib, megaparsec, parallel, text }:
mkDerivation {
  pname = "day07";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec parallel text ];
  executableHaskellDepends = [ base text ];
  license = lib.licenses.bsd3;
  mainProgram = "day07";
}
