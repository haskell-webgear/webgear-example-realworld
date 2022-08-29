{ mkDerivation, aeson, arrows, base, base64-bytestring, bytestring
, bytestring-conversion, http-api-data, http-media, http-types
, jose, lib, monad-time, mtl, QuickCheck, quickcheck-instances
, tasty, tasty-hunit, tasty-quickcheck, text, unordered-containers
, wai, webgear-core
}:
mkDerivation {
  pname = "webgear-server";
  version = "1.0.4";
  sha256 = "3e10301b887f5a89d3ec7a83443d48dd99171d37418cf5eca0172b9df276d680";
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
