{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    # old nixpkgs with elm 0.19.0
    nixpkgs-elm0190.url = "github:nixos/nixpkgs?rev=22b0be560914b738e5342148caebd5c575b5a0b9";
    nixpkgs-elm0190.flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs-elm0190, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.gh
              pkgs.elmPackages.elm
              pkgs.elmPackages.elm-format
              pkgs.elmPackages.elm-test
            ];
          };
          # elm-0.19.1 cannot be used to publish elm-geometry,
          # because the generated `docs.json` is very large.
          # We workaround this using a dev shell with elm-0.19.0
          # `nix develop .#publish`
          publish =
            let
              elmNixpkgsPrefix = "${nixpkgs-elm0190}/pkgs/development/compilers/elm";
              elm0190 = with pkgs.haskell.lib.compose; justStaticExecutables (overrideCabal
                (drv: {
                  enableParallelBuilding = false;
                  jailbreak = true;
                  preConfigure = pkgs.callPackage "${elmNixpkgsPrefix}/fetchElmDeps.nix" { } {
                    elmPackages = import "${elmNixpkgsPrefix}/packages/elm-srcs.nix";
                    versionsDat = "${elmNixpkgsPrefix}/versions.dat";
                  };
                  patches = [
                    (pkgs.fetchpatch {
                      url = "https://github.com/elm/compiler/pull/1886/commits/39d86a735e28da514be185d4c3256142c37c2a8a.patch";
                      sha256 = "0nni5qx1523rjz1ja42z6z9pijxvi3fgbw1dhq5qi11mh1nb9ay7";
                    })
                  ];
                })
                (with pkgs.haskell.packages.ghc810; callPackage "${elmNixpkgsPrefix}/packages/elm.nix" {
                  # re-add `lib` to `stdenv` for backwards compatibility with old nixpkgs
                  stdenv = pkgs.stdenv // { lib = pkgs.lib; };
                  # fix "Module ‘Control.Monad.Logic’ does not export ‘lift’"
                  logict = callHackage "logict" "0.6.0.3" { };
                }));
            in
            pkgs.mkShell {
              buildInputs = [ elm0190 ];
            };
        };
      });
}
