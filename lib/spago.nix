{ pkgs, inputs, self, ... }:

let
  inherit (pkgs) lib;

  eps = import inputs.easy-purescript-nix { inherit pkgs; };

  utils = import ./utils.nix { inherit pkgs; };

  # Generate the project specification from a `packages.dhall` through the
  # magic of IFD.
  #
  # This will contain an `upstream` package set (e.g. `psc-0.15.3`) and a list
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
      # Paths to configuration files describing the build: `packages.dhall`,
      # etc... These can be derived from the `src`, but can also be explicitly
      # passed
    , buildConfig ? {
        packagesDhall = /. + src + "/packages.dhall";
      }
    , ...
    }:
    let
      plan = projectPlanFor name buildConfig.packagesDhall;

      # Get the upstream package set
      upstream = import
        (../package-sets + "/${plan.upstream.path}.nix")
        { inherit pkgs; };

      # TODO Add the `additional` as well later
      spagoPkgs = upstream;

      installed = pkgs.runCommand
        "install-spago-deps"
        {
          nativeBuildInputs = [ (utils.install spagoPkgs) ];
        }
        ''
          mkdir $out && cd $out
          install-spago-pkgs
        '';

      # Make a `devShell` from the options provided via `spagoProject.shell`,
      # all of which have default options
      mkDevShell =
        {
          # List of purescript development tools available from
          # `easy-purescript-nix`
          tools ? { }
          # Extra packages to include in the development environment
        , packages ? [ ]
          # If `true`, the Spago packages will be installed in `./.spago`
        , install ? true
        , shellHook ? ""
        }:
        let
          # First convert the compiler version from above into a format
          # suitable for importing from `easy-purescript-nix`
          compilerVersion = builtins.replaceStrings
            [ "." ] [ "_" ]
            (compilerVersionFor plan);
        in
        pkgs.mkShell
          {
            inherit packages;
            nativeBuildInputs = [
              # Get the correct version of the `purs` compiler
              eps."purs-${compilerVersion}"
              eps.spago
            ];
            shellHook =
              (
                lib.optionalString install
                  ''
                    dest=./.spago
                    if [ -L "$dest" ]; then
                      unlink "$dest"
                    fi
                    ln -s ${installed}/.spago ./.spago
                  ''
              )
              + shellHook;
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
