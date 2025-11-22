{ mkDerivation, base, containers, hspec, hspec-megaparsec, lib
, megaparsec, tasty, tasty-hspec, text
}:
mkDerivation {
  pname = "day05";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec text ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base hspec hspec-megaparsec megaparsec tasty tasty-hspec text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "day05";
}
