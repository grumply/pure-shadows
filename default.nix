{ mkDerivation, stdenv, ghc, base, pure-styles, pure-txt
}:
mkDerivation {
  pname = "pure-shadows";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-styles pure-txt ];
  homepage = "github.com/grumply/pure-shadows";
  license = stdenv.lib.licenses.bsd3;
}
