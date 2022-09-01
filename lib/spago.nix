{ pkgs, inputs, self, ... }:

{
  spagoProject = import ./project.nix { inherit self inputs pkgs; };

  # Utilities for generating flake outputs, etc...
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
        # flake app
        fromNodeApp = app: appHelper "${app}/bin/${app.name}";

        # Turn a NodeJS app (e.g. using `nodeApp` from `spagoProject`) into a
        # flake app and run it with the arguments provided to `nix run`
        #
        # NOTE: You must include the command name as the first argument, e.g.
        # `nix run -- cmd --arg1 --arg2 ...`
        fromNodeAppWithArgs = app: appHelper ''${app}/bin/${app.name} "$@"'';

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
