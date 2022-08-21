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
      # A set mapping dependency names to the specific revision and sha256 hash
      # of its sources. This is required as `packages.dhall` does not
      # allow you to specify the hashes of git dependencies. You can use
      # `nix-prefetch-git` at the given revision to calculate the hash or use
      # flake inputs and set the hash to the `input.name.narHash`
      #
      # For example:
      # ```
      #   sha256map = {
      #     foo = {
      #       rev = "9d34557edc0e574665ac60978d42d93f2445d3be";
      #       sha256 = "1zhq9srrdr645q5cgdbglp6y2d9mkn6zci8qhcyxh5drf1ardy7d";
      #     };
      #   };
      # ```
      #
      # NOTE: For Spago projects that consist solely of upstream, first-party
      # dependencies, this can be left empty
    , sha256map ? { }
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

      additional = lib.trivial.flip builtins.mapAttrs plan.additions
        (
          name: dep: pkgs.stdenv.mkDerivation {
            inherit name;
            inherit (dep) version;
            phases = "installPhase";
            installPhase = "ln -s $src $out";
            src = pkgs.fetchgit {
              rev = sha256map.${name}.rev;
              sha256 = sha256map.${name}.sha256;
              url = dep.repo;
            };
          }
        );

      spagoPkgs = lib.attrsets.recursiveUpdate upstream additional;

      # Install this once, and then is can be symlinked in later derivations.
      # We can also use this to trick Spago into thinking it has written to its
      # global cache
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
          # If `true`, the Spago packages will be installed in `./.spago` in
          # the `devShell`'s `shellHook`
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
  utils = { inherit (utils) makeSha256map; };
}
