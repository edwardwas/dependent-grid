{compiler ? "default", profiling ? false}:
let
  usedCompiler = if compiler == "default" then "ghc802" else compiler;
  config = {
    permittedInsecurePackages = [
             "webkitgtk-2.4.11"
    ];
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.${usedCompiler}.override {
        overrides = self : super : rec {
          dependent-grid = callCabal2nix "dependent-grid" ./. {};
          ghc = super.ghc // {withPackages = super.ghc.withHoogle;};
          ghcWithPackages = self.ghc.withPackages;
          mkDerivation = args: super.mkDerivation (args // {
            enableLibraryProfiling = false;
          });
        };
      };
    };
  };

  pkgs = import <nixpkgs> {inherit config;};
  callCabal2nix = pkgs.haskellPackages.callCabal2nix;
  callHackage = pkgs.haskellPackages.callHackage;
  hLib = pkgs.haskell.lib;

in
  pkgs.haskellPackages.dependent-grid
