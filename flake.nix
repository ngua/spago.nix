{
  description = "spago.nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-parts, ... }@inputs:
    let
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        flake-parts.flakeModules.easyOverlay
      ];

      perSystem = { config, pkgs, lib, system, ... }:
        let
          hsProjectName = "pure-spago-nix";
          mkHsProject =
            { returnShellEnv ? false
            , compiler ? "ghc924"
            , modifier ? lib.id
            , ...
            }:
            let
              p = pkgs.haskell.packages.${compiler}.developPackage {
                inherit modifier returnShellEnv;
                root = self + /${hsProjectName};
              };
            in
            p.overrideAttrs (_: { passthru = { inherit compiler; }; });
        in
        {
          packages = rec {
            default = mkHsProject { };
            "${hsProjectName}" = default;
          };

          devShells = {
            default =
              let
                inherit (hsp.passthru) compiler;
                hsp = mkHsProject {
                  returnShellEnv = true;
                  modifier = drv:
                    pkgs.haskell.lib.addBuildTools drv
                      (
                        with pkgs;
                        with haskell.packages.${compiler};
                        [
                          fourmolu
                          haskell-language-server
                          cabal-install
                          hlint
                          wget
                          nixpkgs-fmt
                          nix-prefetch-git
                          self.packages.${system}.pure-spago-nix
                        ]
                      )
                  ;
                };
              in
              hsp;
          };

          overlayAttrs = {
            spago-nix = import ./lib/spago.nix {
              inherit self inputs pkgs;
            };
          };
        };
    };
}
