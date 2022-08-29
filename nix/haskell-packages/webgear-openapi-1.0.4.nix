{ mkDerivation, arrows, base, http-media, http-types
, insert-ordered-containers, lens, lib, openapi3, text
, webgear-core
}:
mkDerivation {
  pname = "webgear-openapi";
  version = "1.0.4";
  sha256 = "91b800bc100444fbd13ed0084c76efc9e56940a6d560a5e005964c750ebe0f73";
  libraryHaskellDepends = [
    arrows base http-media http-types insert-ordered-containers lens
    openapi3 text webgear-core
  ];
  homepage = "https://github.com/haskell-webgear/webgear#readme";
  description = "Composable, type-safe library to build HTTP API servers";
  license = lib.licenses.mpl20;
}
