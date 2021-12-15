{ mkDerivation, base, comonad, lib, matrix, optparse-applicative
, time, vector
}:
mkDerivation {
  pname = "sucodo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base comonad matrix optparse-applicative time vector
  ];
  license = lib.licenses.bsd3;
}
