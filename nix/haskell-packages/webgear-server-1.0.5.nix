{ mkDerivation, aeson, arrows, base, base64-bytestring, bytestring
, bytestring-conversion, http-api-data, http-media, http-types
, jose, lib, monad-time, mtl, QuickCheck, quickcheck-instances
, tasty, tasty-hunit, tasty-quickcheck, text, unordered-containers
, wai, webgear-core
}:
mkDerivation {
  pname = "webgear-server";
  version = "1.0.5";
  sha256 = "81e3143e04699802dcbe7dc17962c52c65ef73130fc5167008f56ce9f25e4cd1";
  libraryHaskellDepends = [
    aeson arrows base base64-bytestring bytestring
    bytestring-conversion http-api-data http-media http-types jose
    monad-time mtl text unordered-containers wai webgear-core
  ];
  testHaskellDepends = [
    base base64-bytestring bytestring http-types QuickCheck
    quickcheck-instances tasty tasty-hunit tasty-quickcheck text wai
    webgear-core
  ];
  homepage = "https://github.com/haskell-webgear/webgear#readme";
  description = "Composable, type-safe library to build HTTP API servers";
  license = lib.licenses.mpl20;
}
