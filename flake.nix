{
  description = "purescript.nix";

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
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = nixpkgs.lib.attrValues self.overlays ++
          [
            (_: _: {
              pure-spago-nix = self.packages.${system}.pure-spago-nix;
            })
          ];
      };
      hsPackageName = "pure-spago-nix";
      hsProjectFor = system: { returnShellEnv ? false }:
        (nixpkgsFor system).haskell.packages.${compiler}.developPackage {
          inherit returnShellEnv;
          root = "${self}/${hsPackageName}";
        };
    in
    {
      packages = perSystem (system: {
        "${hsPackageName}" = (hsProjectFor system) { };
        default = self.packages.${system}.${hsPackageName};
      });

      devShell = perSystem (system:
        ((hsProjectFor system) { returnShellEnv = true; }).overrideAttrs
          (oas:
            let
              pkgs = nixpkgsFor system;
            in
            {
              buildInputs = oas.buildInputs ++
                (with pkgs.haskell.packages.${compiler}; [
                  fourmolu
                  haskell-language-server
                  cabal-install
                  hlint
                  pkgs.wget
                  pkgs.nixpkgs-fmt
                  pkgs.pure-spago-nix
                  pkgs.dhall-nix
                  pkgs.nix-prefetch-git
                ]);
            })
      );

      overlays = {
        default = import ./overlay.nix {
          inherit inputs;
        };
      };
    };
}
