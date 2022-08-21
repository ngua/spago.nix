{
  description = "Simple purescript.nix example";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # TODO use remote once it exists
    spago-nix.url = "git+file:///home/rory/projects/spago.nix";

    # For use with the `sha256map` to create a project
    lattice = {
      url = "github:Risto-Stevcev/purescript-lattice/v0.3.0";
      flake = false;
    };
    properties = {
      url = "github:Risto-Stevcev/purescript-properties/v0.2.0";
      flake = false;
    };

  };
  outputs = { self, nixpkgs, spago-nix, ... }@inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ spago-nix.overlays.default ];
      };
      projectFor = pkgs: pkgs.spago-nix.spagoProject {
        name = "spago-nix-example";
        src = ./.;
        shell = {
          tools = [ "psa" ];
        };
        buildConfig = {
          packagesDhall = ./packages.dhall;
        };
        sha256map = pkgs.spago-nix.utils.makeSha256map inputs;
      };
      flakeFor = system: (projectFor (nixpkgsFor system)).flake;
    in
    {
      devShell = perSystem (system: (flakeFor system).devShells.default);
    };
}
