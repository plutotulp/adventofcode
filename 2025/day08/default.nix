{ mkDerivation, base, containers, lib, megaparsec, mtl, text }:
mkDerivation {
  pname = "day08";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec mtl text ];
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "day08";
}
