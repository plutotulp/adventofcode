{ mkDerivation, base, containers, lib, mtl, parallel, text }:
mkDerivation {
  pname = "day06";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers mtl parallel ];
  executableHaskellDepends = [ base text ];
  license = lib.licenses.bsd3;
  mainProgram = "day06";
}
