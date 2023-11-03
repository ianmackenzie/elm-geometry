{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    # nixpkgs with elm 0.19.0
    nixpkgs-elm0190.url = "github:nixos/nixpkgs?rev=22b0be560914b738e5342148caebd5c575b5a0b9";
    nixpkgs-elm0190.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, nixpkgs-elm0190 }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        elm-nix-src = "${nixpkgs-elm0190}/pkgs/development/compilers/elm";
        ghc810Pkgs = pkgs.haskell.packages.ghc810.override {
          overrides = self: super: with pkgs.haskell.lib.compose;
            rec {
              elm = justStaticExecutables (overrideCabal
                (drv: {
                  enableParallelBuilding = false;
                  preConfigure = pkgs.callPackage "${elm-nix-src}/fetchElmDeps.nix" { } {
                    elmPackages = import "${elm-nix-src}/packages/elm-srcs.nix";
                    versionsDat = "${elm-nix-src}/versions.dat";
                  };
                  patches = [
                    (pkgs.fetchpatch {
                      url = "https://github.com/elm/compiler/pull/1886/commits/39d86a735e28da514be185d4c3256142c37c2a8a.patch";
                      sha256 = "0nni5qx1523rjz1ja42z6z9pijxvi3fgbw1dhq5qi11mh1nb9ay7";
                    })
                  ];
                  jailbreak = true;
                })
                (self.callPackage "${elm-nix-src}/packages/elm.nix" {
                  stdenv = pkgs.stdenv // { lib = pkgs.lib; };
                }));
              # fix "Module ‘Control.Monad.Logic’ does not export ‘lift’"
              logict = self.callHackage "logict" "0.6.0.3" { };
            };
        };
      in
      {
        packages.default = ghc810Pkgs.elm;
      });
}
