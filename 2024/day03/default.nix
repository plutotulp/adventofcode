{ mkDerivation, base, file-embed, hspec, hspec-megaparsec, lib
, megaparsec, mtl, tasty, tasty-hspec, text
}:
mkDerivation {
  pname = "day03";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base megaparsec mtl text ];
  executableHaskellDepends = [ base file-embed text ];
  testHaskellDepends = [
    base file-embed hspec hspec-megaparsec megaparsec tasty tasty-hspec
    text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "day03";
}
