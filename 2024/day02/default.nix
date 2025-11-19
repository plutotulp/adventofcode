{ mkDerivation, base, file-embed, lib, megaparsec, text }:
mkDerivation {
  pname = "day02";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base file-embed megaparsec text ];
  license = lib.licenses.bsd3;
  mainProgram = "day02";
}
