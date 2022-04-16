{
  description = "Simple purescript.nix example";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # TODO use remote once it exists
    purescript-nix.url = "git+file:///home/rory/projects/purescript.nix";
  };
  outputs = { self, nixpkgs, purescript-nix, ... }:
    let
      defaultSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      perSystem = nixpkgs.lib.genAttrs defaultSystems;
      nixpkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ purescript-nix.overlay ];
      };
      psProjectFor = system:
        (nixpkgsFor system).purescript-nix.purescriptProject {
          compiler = "0.14.5";
          shell = {
            tools = [ "purs-tidy" ];
          };
        };
      psFlake = system: (psProjectFor system).flake;
    in
    {
      devShell = perSystem (system: (psFlake system).devShell);
    };
}
