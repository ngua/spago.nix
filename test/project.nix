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
          (builtins.readFile ../example/flake.lock);
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
  testFor = { src, addExtraInputs ? true }:
    let
      project = pkgs.spago-nix.spagoProject
        (
          {
            inherit src;
            name = "spago-nix-test-${builtins.baseNameOf src}";
            shell = {
              tools = {
                psa = { };
                purescript-language-server = "0.17.1";
                purs-tidy = "latest";
              };
            };
          } // lib.optionalAttrs addExtraInputs {
            extraSources = { inherit (extraInputs) lattice properties; };
          }
        );

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
