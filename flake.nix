{
  description = "WebGear example project - RealWorld App";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      # Use the same nixpkgs
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, gitignore }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ haskellOverlay ];
        };
        ghcVersion = "ghc944";
        hsPkgs = pkgs.haskell.packages.${ghcVersion};

        pkgName = "webgear-example-realworld";

        haskellOverlay = final: prev: {
          haskell = prev.haskell // {
            packages = prev.haskell.packages // {
              ${ghcVersion} = prev.haskell.packages.${ghcVersion}.override {
                overrides = hfinal: hprev: {
                  # Broken packages and dependencies
                  bytestring-conversion = hfinal.callPackage ./nix/haskell-packages/bytestring-conversion-0.3.2.nix {};
                  generics-sop = hfinal.callPackage ./nix/haskell-packages/generics-sop-0.5.1.3.nix {};
                  openapi3 = hfinal.callPackage ./nix/haskell-packages/openapi3-3.2.3.nix {};
                  persistent = hfinal.callPackage ./nix/haskell-packages/persistent-2.14.5.0.nix {};
                  postgresql-simple = hfinal.callPackage ./nix/haskell-packages/postgresql-simple-0.6.5.nix {};

                  webgear-core = hfinal.callPackage ./nix/haskell-packages/webgear-core-1.0.5.nix {};
                  webgear-server = hfinal.callPackage ./nix/haskell-packages/webgear-server-1.0.5.nix {};
                  webgear-openapi = hfinal.callPackage ./nix/haskell-packages/webgear-openapi-1.0.5.nix {};
                  ${pkgName} = hfinal.callCabal2nix pkgName (gitignore.lib.gitignoreSource ./.) {};
                };
              };
            };
          };
        };
      in {
        packages.default = hsPkgs.${pkgName};
        devShells.default = hsPkgs.shellFor {
          name = pkgName;
          packages = pkgs: [ pkgs.${pkgName} ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
            hsPkgs.fourmolu
            hsPkgs.ghc
            pkgs.hlint
            pkgs.haskell-language-server
          ];
          src = null;
        };
      });
}
