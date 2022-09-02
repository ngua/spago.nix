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

            # `docs/README.org` contains an `INCLUDE` directive to insert the
            # contents of an example flake.nix. This won't be rendered on GH
            # though, so instead it's exported here to a root-level `README.org`,
            # which will inline the text from the example
            shellHook = ''
              emacs docs/README.org --batch -f org-org-export-to-org --kill
            '';
          })
      );

      overlays = {
        default = final: _: {
          spago-nix = import ./lib/spago.nix {
            inherit self inputs;
            pkgs = final;
          };
        };
      };
    };
}
