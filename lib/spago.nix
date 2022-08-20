{ pkgs, inputs, self, ... }:

let
  inherit (pkgs) lib;

  easy-ps = import inputs.easy-purescript-nix { inherit pkgs; };

  spagoProject =
    { name
    , shell ? { }
    , ...
    }:
    let
      # Make a `devShell` from the options provided via `spagoProject.shell`,
      # all of which have default options
      #
      # TODO
      mkDevShell =
        {
          # List of purescript development tools available from
          # `easy-purescript-nix`
          tools ? { }
          # Extra packages to include in the development environment
        , buildInputs ? [ ]
        }: pkgs.mkShell {
          inherit buildInputs;
        };
    in
    rec {
      devShell = mkDevShell shell;

      flake = { devShells.default = devShell; };
    };

in
{
  spago-nix = {
    inherit spagoProject;
  };
}
