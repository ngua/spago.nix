{ pkgs, inputs, self, ... }:

rec {
  spagoProject = import ./project.nix {
    inherit self inputs pkgs utils;
  };

  # Utilities for generating flake outputs, etc...
  #
  # This can be helpful outside of the context of `spagoProject` as well, so
  # it's defined here separately (e.g. the functions can be applied to existing
  # derivations not taken from `spagoProject`)
  utils = {
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
        fromNodeApp = { app }: appHelper ''${app}/bin/${app.name} "$@"'';

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
  };
}
