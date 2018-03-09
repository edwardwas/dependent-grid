{compiler ? "default", profiling ? false}:
let
  usedCompiler = if compiler == "default" then "ghc802" else compiler;
  config = doProfile : {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${usedCompiler}.override {
        overrides = self : super : rec {
          dependent-grid = callCabal2nix "dependent-grid" ./. {};
          ghc = super.ghc // {withPackages = super.ghc.withHoogle;};
          ghcWithPackages = self.ghc.withPackages;
          mkDerivation = args: super.mkDerivation (args // {
            enableLibraryProfiling = doProfile;
          });
        };
      };
    };
  };

  pkgs = import <nixpkgs> { config = config profiling; };
  utilPkgs = import <nixpkgs> { config = config false;};
  callCabal2nix = utilPkgs.haskellPackages.callCabal2nix;
  callHackage = utilPkgs.haskellPackages.callHackage;
  hLib = pkgs.haskell.lib;

in
  pkgs.haskellPackages.dependent-grid
