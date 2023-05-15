{ mkDerivation, arrows, base, bytestring, case-insensitive
, filepath, http-api-data, http-media, http-types, jose, lib
, mime-types, network, safe-exceptions, tagged, template-haskell
, text, unordered-containers, wai
}:
mkDerivation {
  pname = "webgear-core";
  version = "1.0.5";
  sha256 = "4c182fbdcc3d1ff56a971ecf11a7d01b27654c676e1d725a8103cb747729ded0";
  libraryHaskellDepends = [
    arrows base bytestring case-insensitive filepath http-api-data
    http-media http-types jose mime-types network safe-exceptions
    tagged template-haskell text unordered-containers wai
  ];
  homepage = "https://github.com/haskell-webgear/webgear#readme";
  description = "Composable, type-safe library to build HTTP APIs";
  license = lib.licenses.mpl20;
}
