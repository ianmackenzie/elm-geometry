{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    elm.url = "./elm-0.19.0";
    elm.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, elm, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells = {
          default = pkgs.mkShell {
            buildInputs = [
              pkgs.elmPackages.elm
              pkgs.elmPackages.elm-format
              pkgs.elmPackages.elm-test
            ];
          };
          publish = pkgs.mkShell {
            buildInputs = [ elm.packages.${system}.default ];
          };
        };
      });
}
