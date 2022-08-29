{ mkDerivation, arrows, base, bytestring, case-insensitive
, filepath, http-api-data, http-media, http-types, jose, lib
, mime-types, network, safe-exceptions, tagged, template-haskell
, text, unordered-containers, wai
}:
mkDerivation {
  pname = "webgear-core";
  version = "1.0.4";
  sha256 = "875aa076ce748146c4249e5d413b700127df76f701594e214d94a2b0ed0f002a";
  libraryHaskellDepends = [
    arrows base bytestring case-insensitive filepath http-api-data
    http-media http-types jose mime-types network safe-exceptions
    tagged template-haskell text unordered-containers wai
  ];
  homepage = "https://github.com/haskell-webgear/webgear#readme";
  description = "Composable, type-safe library to build HTTP APIs";
  license = lib.licenses.mpl20;
}
