{
  description = "Simple purescript.nix example";
  inputs = {
    nixpkgs.follows = "spago-nix/nixpkgs";
    spago-nix.url = "github:ngua/spago.nix";
    flake-utils.url = "github:numtide/flake-utils";

    # Additional Purescript dependencies can be pinned in your flake `inputs`
    # and then provided to `spagoProject` via `extraSources` (see below)
    lattice = {
      url = "github:Risto-Stevcev/purescript-lattice/v0.3.0";
      flake = false;
    };
    properties = {
      url = "github:Risto-Stevcev/purescript-properties/v0.2.0";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , spago-nix
    , flake-utils
    , ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          # This is necessary to access the various functionality that `spago.nix`
          # provides. The entire interface is exposed via the `spago-nix` prefix
          # in the resulting package set
          overlays = [ spago-nix.overlays.default ];
        };
        # `spago-nix.spagoProject` is the key function for building your Spago
        # project with Nix. It provides various attributes, some of which are
        # demonstrated below (see `./docs/reference.org` for all of them)
        project = pkgs.spago-nix.spagoProject {
          name = "spago-nix-example";
          src = ./.;
          # These are third-party dependencies that don't exist in the upstream
          # package set. The pinned inputs *must* match the exact revision that
          # is described in your `packages.dhall`, otherwise your project likely
          # won't compile
          extraSources = { inherit (inputs) lattice properties; };
          # This is used to generate a `devShell`. See `./docs/reference.org` for
          # all of the available options
          shell = {
            tools = [ "psa" ];
          };
        };
      in
      {
        # `spagoProject` returns, among other things, a `flake` attribute with
        # some pre-built outputs for your convenience
        devShells = { inherit (project.flake.devShells) default; };
        # `flake.packages` contains the compiled `output` and `docs`. Since
        # Spago does has no mechanism for defining components in your config,
        # `spagoProject` also returns functions for creating the derivations
        # that cannot be generated for you automatically
        packages = project.flake.packages // {
          bundled-module = project.bundleModule { main = "Main"; };
          bundled-app = project.bundleApp { main = "Main"; };
          node-app = project.nodeApp { main = "Main"; };
        };
        # Similarly, `flake.apps` contains a `docs` app that serves the documentation
        # from a webserver on `localhost`
        apps = project.flake.apps // {
          # `spago-nix.utils` has some helpers for reusing existing `packages`
          # to create `apps`
          node-app = pkgs.spago-nix.utils.apps.fromNodeApp {
            app = self.packages.${system}.node-app;
          };
        };
        checks.default = project.runTest { testMain = "Main"; };
      }
    );
}
