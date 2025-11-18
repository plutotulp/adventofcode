{ mkDerivation, base, file-embed, lib, megaparsec
, parser-combinators, text
}:
mkDerivation {
  pname = "day02";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base file-embed megaparsec parser-combinators text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "day02";
}
