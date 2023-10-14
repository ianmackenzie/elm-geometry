{
  inputs = {
    # nixpkgs with ghc865
    nixpkgs.url = "github:nixos/nixpkgs?rev=795ef4f46f66c9ca6ab43929135f7ebb6170711b";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        fetchElmDeps = pkgs.callPackage ./fetchElmDeps.nix { };
        ghc865Pkgs = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib;
            rec {
              elm = justStaticExecutables (overrideCabal (self.callPackage ./elm.nix { }) (drv: {
                jailbreak = true;
                doCheck = false;
                patches = [
                  (pkgs.fetchpatch {
                    url = "https://github.com/elm/compiler/pull/1886/commits/39d86a735e28da514be185d4c3256142c37c2a8a.patch";
                    sha256 = "0nni5qx1523rjz1ja42z6z9pijxvi3fgbw1dhq5qi11mh1nb9ay7";
                  })
                ];
                preBuild = ''
                  export HOME=$(pwd)
                '';
                preConfigure = fetchElmDeps {
                  elmPackages = (import ./elm-srcs.nix);
                  elmVersion = drv.version;
                  registryDat = ./registry.dat;
                };
              }));
            };
        };
      in
      { packages.default = ghc865Pkgs.elm; });
}
