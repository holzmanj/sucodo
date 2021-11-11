{ mkDerivation, base, comonad, lib, matrix, vector }:
mkDerivation {
  pname = "sucodo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base comonad matrix vector ];
  license = lib.licenses.bsd3;
}
