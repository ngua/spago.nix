{
  description = "Simple purescript.nix example";
  inputs = {
    nixpkgs.follows = "spago-nix/nixpkgs";
    spago-nix.url = "github:ngua/spago.nix";

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
      perSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ spago-nix.overlays.default ];
      };
      projectFor = pkgs: pkgs.spago-nix.spagoProject {
        name = "spago-nix-example";
        src = ./.;
        extraSources = { inherit (inputs) lattice properties; };
        shell = {
          tools = [ "psa" ];
        };
      };
      flakeFor = system: (projectFor (nixpkgsFor system)).flake;
    in
    {
      devShells = perSystem (system: {
        inherit ((flakeFor system).devShells) default;
      });

      packages = perSystem (system:
        let
          project = projectFor (nixpkgsFor system);
        in
        {
          inherit ((flakeFor system).packages) output;
          bundled-module = project.bundleModule { main = "Main"; };
          bundled-app = project.bundleApp { main = "Main"; };
          node-app = project.nodeApp { main = "Main"; };
        });
    };
}
