{
  description = "spago.nix";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      perSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      compiler = "ghc924";
      nixpkgsFor = system: nixpkgs.legacyPackages.${system};
      hsPackageName = "pure-spago-nix";
      hsProjectFor = system: { returnShellEnv ? false }:
        (nixpkgsFor system).haskell.packages.${compiler}.developPackage {
          inherit returnShellEnv;
          root = self + "/${hsPackageName}";
        };
    in
    {
      packages = perSystem (system: {
        "${hsPackageName}" = (hsProjectFor system) { };
        default = self.packages.${system}.${hsPackageName};
      });

      devShell = perSystem (system:
        ((hsProjectFor system) { returnShellEnv = true; }).overrideAttrs
          (oas: {
            buildInputs =
              with (nixpkgsFor system);
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
              ++ oas.buildInputs;
          })
      );

      overlays = {
        default = import ./overlay.nix {
          inherit inputs self;
        };
      };
    };
}
