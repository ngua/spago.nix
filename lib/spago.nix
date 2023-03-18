{ pkgs
, self
, ...
}:

rec {
  spagoProject = import ./project.nix {
    inherit self pkgs utils;
  };

  # Utilities for generating flake outputs, etc...
  #
  # This can be helpful outside of the context of `spagoProject` as well, so
  # it's defined here separately (e.g. the functions can be applied to existing
  # derivations not taken from `spagoProject`)
  utils = {
    mkTool = tool: v:
      let
        getPackage = {
          set = tool;
          string = {
            latest = tool;
          }.${v} or "${tool}-${pkgs.purescriptPackages.lib.shortVersion v}";
        }.${builtins.typeOf v};
      in
        pkgs.purescriptPackages.${getPackage} or
          (
            builtins.throw
              (
                "Invalid tool version (${builtins.toString v}) specified for ${tool};"
                + " pass `latest` or `{ }` for latest version, or version string if"
                + " the specific version exists in your package set"
              )
          );
    apps =
      let
        appHelper = program: {
          inherit program;
          type = "app";
        };
      in
      {
        # Turn a NodeJS app (e.g. using `nodeApp` from `spagoProject`) into a
        # flake app. If you pass arguments to the `nix run` invocation, then these
        # will be passed to the application.
        #
        # NOTE: If you do pass command-line arguments, you should provide the
        # command name as the first argument (or see below for another way)
        #
        # NOTE: Using `nodeAppWithArgs` will always use the `arguments`
        # provided to that derivation (and not any command-line arguments), so
        # it is not appropriate for use with this helper. You can, however,
        # fix the command's name (the first element of Node's `process.env.argv`),
        # and pass an empty list of `arguments` (the default value), e.g.:
        #
        # ```
        # node-app = pkgs.spago-nix.utils.fromNodeApp {
        #   app = project.nodeAppWithArgs {
        #     main = "The.Main.Module";
        #     # or you can leave it empty, a default will be created
        #     command = "the-command";
        #     # this is the default
        #     arguments = [ ];
        #   };
        # }
        # ```
        #
        fromNodeApp = { app }: appHelper ''${app}/bin/${app.name}'';

        fromDocs = { docs, port ? 8080 }:
          let
            server = pkgs.writeShellApplication {
              name = "docs-server";
              runtimeInputs = [
                pkgs.nodejs-14_x
                pkgs.nodePackages.http-server
              ];
              text =
                ''
                  http-server --port ${builtins.toString port} ${docs}/generated-docs
                '';
            };
          in
          appHelper "${server}/bin/${server.name}";
      };
    # Javascript-specific utilities
    js = {
      # Generates `node_modules`. `spagoProject` does this internally, but some
      # users may wish to override the `nodeModules` that get used in different
      # derivations (for example, to include/exclude development dependencies)
      nodeModulesFor =
        { name ? "" # Appended to name of `runCommand` invocation
          # Path to `package.json`
        , packageJson
          # Path to `package-lock.json`
        , packageLock
          # If `devDependencies` should be included
        , development ? true
        , nodejs ? pkgs.nodejs-14_x
        }:
        let
          nodeEnv = import
            (
              pkgs.runCommand "node-modules-${name}"
                {
                  buildInputs = [ pkgs.nodePackages.node2nix ];
                }
                ''
                  mkdir $out && cd $out
                  cp ${packageLock} ./package-lock.json
                  cp ${packageJson} ./package.json
                  node2nix ${pkgs.lib.optionalString development "--development" } \
                    --lock ./package-lock.json -i ./package.json
                ''
            )
            {
              inherit pkgs nodejs;
              inherit (pkgs) system;
            };
          modules = pkgs.callPackage
            (
              _:
              nodeEnv // {
                shell = nodeEnv.shell.override {
                  # see https://github.com/svanderburg/node2nix/issues/198
                  buildInputs = [ pkgs.nodePackages.node-gyp-build ];
                };
              }
            );
        in
        (modules { }).shell.nodeDependencies;
    };
  };
}
