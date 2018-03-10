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
          reflex = self.callPackage ./nix/reflex.nix {};
          reflex-dom-core = self.callPackage ./nix/reflex-dom-core.nix {};
          reflex-dom = self.callPackage ./nix/reflex-dom.nix {};
          reflex-dom-contrib = self.callPackage ./nix/reflex-dom-contrib.nix {};
          haskell-gi-overloading = self.callPackage ./nix/haskell-gi-overloading.nix {};
          blank-canvas = self.callPackage ./nix/blank-canvas.nix {};
          diagrams-reflex = self.callPackage ./nix/diagrams-reflex.nix {};
          jsaddle-warp = self.callPackage ./nix/jsaddle-warp.nix {};
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
