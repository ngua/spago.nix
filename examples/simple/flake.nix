{
  description = "Simple purescript.nix example";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # TODO use remote once it exists
    spago-nix.url = "git+file:///home/rory/projects/spago.nix";
  };
  outputs = { self, nixpkgs, spago-nix, ... }:
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
        shell = { };
      };
      flakeFor = system: (projectFor (nixpkgsFor system)).flake;
    in
    {
      devShell = perSystem (system: (flakeFor system).devShells.default);
    };
}
