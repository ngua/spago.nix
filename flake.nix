{
  description = "spago.nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-parts, treefmt-nix, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        flake-parts.flakeModules.easyOverlay
        treefmt-nix.flakeModule
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
                root = builtins.path {
                  name = "${hsProjectName}-src";
                  path = "${self}/${hsProjectName}";
                };
              };
            in
            p.overrideAttrs (_: { passthru = { inherit compiler; }; });
        in
        rec {
          legacyPackages = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          };

          packages = rec {
            default = mkHsProject { };
            "${hsProjectName}" = default;
          };

          checks = {
            statix = pkgs.runCommand "statix-check"
              {
                nativeBuildInputs = [ pkgs.statix ];
              }
              ''
                cd ${self}
                statix check
                touch $out
              '';
            projects =
              let
                test = import ./test/project.nix {
                  pkgs = self.legacyPackages.${system};
                };
              in
              pkgs.runCommand "projects-check"
                {
                  projects = builtins.concatMap
                    (x: builtins.attrValues (test.testFor x))
                    [
                      ./examples/v0.14
                      ./examples/v0.15
                      ./examples/with
                    ];
                }
                ''
                  echo $projects
                  touch $out
                '';
          };

          apps = {
            update-examples =
              let
                name = "update-examples";
                update = loc:
                  ''
                    pushd ${loc}
                    nix flake lock --update-input spago-nix
                    popd
                  '';
                script = pkgs.writeShellApplication {
                  inherit name;
                  runtimeInputs = [ pkgs.nix ];
                  text = ''
                    ${update "./examples/simple"}
                  '';
                };
              in
              {
                type = "app";
                program = "${script}/bin/${name}";
              };
            generate-package-sets =
              let
                name = "generate-package-sets";
                script = pkgs.writeShellApplication {
                  inherit name;
                  runtimeInputs = [
                    pkgs.wget
                    self.packages.${system}.${hsProjectName}
                  ];
                  text = builtins.readFile ./generate.sh;
                };
              in
              {
                type = "app";
                program = "${script}/bin/${name}";
              };
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

          formatter = treefmt-nix.lib.mkWrapper pkgs treefmt.config;

          treefmt.config = {
            projectRootFile = "flake.nix";
            settings.global.excludes = [ "package-sets/**/*.nix" ];
            programs = {
              nixpkgs-fmt = {
                enable = true;
              };
              ormolu = {
                enable = true;
                package = pkgs.haskellPackages.fourmolu;
              };
            };
          };

          overlayAttrs = {
            spago-nix = import ./lib/spago.nix {
              inherit self inputs pkgs;
            };
          };
        };
    };
}
