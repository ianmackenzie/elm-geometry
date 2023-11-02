{
  inputs = {
    # nixpkgs with ghc865
    nixpkgs-ghc865.url = "github:nixos/nixpkgs?rev=795ef4f46f66c9ca6ab43929135f7ebb6170711b";
    flake-utils.url = "github:numtide/flake-utils";
    # nixpkgs with elm 0.19.0
    nixpkgs-elm0190.url = "github:nixos/nixpkgs?rev=22b0be560914b738e5342148caebd5c575b5a0b9";
    nixpkgs-elm0190.flake = false;
  };

  outputs = { self, nixpkgs-ghc865, flake-utils, nixpkgs-elm0190 }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs-ghc865.legacyPackages.${system};
        elm-nix-src = "${nixpkgs-elm0190}/pkgs/development/compilers/elm";
        ghc865Pkgs = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib;
            rec {
              elm = justStaticExecutables (overrideCabal (self.callPackage "${elm-nix-src}/packages/elm.nix" { }) (drv: {
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
                doCheck = false;
              }));
            };
        };
      in
      { packages.default = ghc865Pkgs.elm; });
}
