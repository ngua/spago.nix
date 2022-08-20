{ pkgs, inputs, self, ... }:

let
  inherit (pkgs) lib;

  eps = import inputs.easy-purescript-nix { inherit pkgs; };

  # Generate the project specification from a `packages.dhall` through the
  # magic of IFD.
  #
  # This will contain an `upstream` package set (e.g. `psc-0.13.3`) and a list
  # of `additional` dependencies. We can then import it to generate the various
  # derivations to build the project, create a suitable `devShell`, etc...
  projectPlanFor = name: packages: import
    (
      pkgs.runCommand
        "${name}-plan"
        {
          nativeBuildInputs = [ self.packages.${pkgs.system}.pure-spago-nix ];
        }
        ''
          mkdir $out
          pure-spago-nix extract ${packages} > $out/default.nix
        ''
    );

  # Get the second component from the specified upstream package set
  # e.g. `psc-0.14.5-20211116` -> `0.14.5`
  #
  # This can be used as the compiler version
  compilerVersionFor = plan: builtins.elemAt
    (lib.strings.splitString "-" plan.upstream.path)
    1;

  spagoProject =
    { name
    , src
    , shell ? { }
    , packages ? /. + src + "/packages.dhall"
    , ...
    }:
    let
      plan = projectPlanFor name packages;
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
        , packages ? [ ]
        }: pkgs.mkShell {
          inherit packages;
          nativeBuildInputs =
            let
              # First convert the compiler version from above into a format
              # suitable for importing from `easy-purescript-nix`
              compilerVersion = builtins.replaceStrings
                [ "." ] [ "_" ]
                (compilerVersionFor plan);
            in
            [
              # Get the correct version of the `purs` compiler
              eps."purs-${compilerVersion}"
            ];
        };
    in
    rec {
      devShell = mkDevShell shell;

      flake = {
        devShells.default = devShell;
        inherit devShell;
      };
    };

in
{
  inherit spagoProject;
}
