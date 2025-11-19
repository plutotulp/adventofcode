{ mkDerivation, base, containers, file-embed, lib, megaparsec, mtl
, text
}:
mkDerivation {
  pname = "day01";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers file-embed megaparsec mtl text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "day01";
}
