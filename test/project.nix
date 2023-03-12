{ pkgs, ... }:

let
  inherit (pkgs) lib;
  # HACK
  # This is a way to get the `extraSources` for the example projects without needing
  # to add them to the `spago.nix` flake itself, which I'd really like to avoid
  extraInputs =
    let
      exampleLock =
        builtins.fromJSON
          (builtins.readFile ../examples/simple/flake.lock);
      getInput = x: exampleLock.nodes.${x}.locked;
      fetchExtraInput = x:
        let
          input = getInput x;
        in
        pkgs.fetchFromGitHub {
          inherit (input) repo owner rev;
          sha256 = input.narHash;
        };
    in
    builtins.listToAttrs
      (
        builtins.map (n: lib.nameValuePair n (fetchExtraInput n))
          [ "lattice" "properties" ]
      );
in
{
  testFor = src:
    let
      project = pkgs.spago-nix.spagoProject {
        inherit src;
        name = "spago-nix-test-${builtins.baseNameOf src}";
        extraSources = { inherit (extraInputs) lattice properties; };
        shell = {
          tools = [ "psa" ];
        };
      };
    in
    {
      inherit (project.flake.packages) output nodeModules docs;
      devShell = project.flake.devShells.default.inputDerivation;
      bundled-module = project.bundleModule { main = "Main"; };
      bundled-app = project.bundleApp { main = "Main"; };
      node-app = project.nodeApp { main = "Main"; };
      test = project.runTest { testMain = "Main"; };
    };
}
