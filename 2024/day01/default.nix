{ mkDerivation, base, containers, file-embed, lib, megaparsec
, parser-combinators, text
}:
mkDerivation {
  pname = "day01";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers file-embed megaparsec parser-combinators text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "day01";
}
