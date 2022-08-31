{ pkgs, inputs, self, ... }:

let
  spagoProject = import ./project.nix {
    inherit self inputs pkgs;
  };
in
{
  inherit spagoProject;

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
      };
  };
}
