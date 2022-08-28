{ self, inputs, pkgs, ... }:
{ name
, src
  # An attribute set mapping dependency names to their source (e.g. from your
  # flake inputs). These must correspond to the overrides or additions in your
  # `packages.dhall`
  #
  # If you provide the sources this way, they will not be fetched from their
  # remote and your do NOT need to provide the sha256 hashes for each extra
  # dependency in the `sha256map`
  #
  # For example (assuming you have added the correct inputs to your flake):
  #
  # ```
  #   extraSources = {
  #     inherit (inputs) foo bar baz;
  #   };
  # ```
  # NOTE: You must pin these to the exact version specified in your
  # `packages.dhall` in order for Spago to find them correctly
  #
  # NOTE: For Spago projects that consist solely of upstream, first-party
  # dependencies, this can be left empty
, extraSources ? { }
  # A set mapping dependency names to the specific revision and sha256 hash
  # of its sources. If you do not use the `extraSources` argument to provide
  # the additional or overriden dependencies specified in your `packages.dhall`,
  # this is required as there is no other way to specify the hashes of git
  # dependencies. You can use `nix-prefetch-git` at the given revision to
  # calculate the hash or use flake inputs and set the hash to the
  # `input.name.narHash`
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
, sha256map ? { }
  # The specific version of `nodejs` to use. Will be used throughout the
  # project components and the shell
, nodejs ? pkgs.nodejs-14_x
, shell ? { }
  # Paths to configuration files describing the build:
  #   - `packages.dhall`
  #   - `spago.dhall`
  #   - `package.json`
  #   - `packages.json`
  # These can be derived from the `src`, but can also be explicitly passed
, buildConfig ? { }
  # Turns on the `--strict` during compilation, e.g. `psa --strict ...`
, strict ? true
  # List of warning types to silence
, censorCodes ? [ ]
  # If `true`, `npm install` will only write to `package-lock.json`
, packageLockOnly ? true
  # If `true`, the project's generate `node_modules` will also contain all
  # `devDependencies`
, development ? true
, ...
}:
let
  inherit (pkgs) lib;

  packagesDhall = buildConfig.packagesDhall or /. + src + "/packages.dhall";
  spagoDhall = buildConfig.spagoDhall or /. + src + "/spago.dhall";
  packageJson = buildConfig.packageJson or /. + src + "/package.json";
  packageLock = buildConfig.packageLock or /. + src + "/package-lock.json";

  eps = import inputs.easy-purescript-nix { inherit pkgs; };

  utils = import ./utils.nix { inherit pkgs; };

  # Generate the project specification from a `packages.dhall` through the
  # magic of IFD.
  #
  # This will contain an `upstream` package set (e.g. `psc-0.15.3`) and a list
  # of `additional` dependencies. We can then import it to generate the various
  # derivations to build the project, create a suitable `devShell`, etc...
  plan = import
    (
      pkgs.runCommand "${name}-plan"
        {
          nativeBuildInputs = [
            self.packages.${pkgs.system}.pure-spago-nix
          ];
        }
        ''
          mkdir $out
          pure-spago-nix extract ${packagesDhall} > $out/default.nix
        ''
    );

  projectNodeModules =
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
              node2nix ${lib.optionalString development "--development" } \
                --lock ./package-lock.json -i ./package.json
            ''
        )
        {
          inherit pkgs nodejs; inherit (pkgs) system;
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

  # Get the correct version of the `purs` compiler
  compiler =
    let
      # Get the second component from the specified upstream package set
      # e.g. `psc-0.14.5-20211116` -> `0.14.5`, then convert it into a format
      # suitable for importing from `easy-purescript-nix`
      version = builtins.replaceStrings
        [ "." ] [ "_" ]
        (
          builtins.elemAt
            (
              lib.strings.splitString "-" plan.upstream.path
            )
            1
        );
    in
    eps."purs-${version}";

  # Get the upstream package set
  upstream = import (../package-sets + "/${plan.upstream.path}")
    { inherit pkgs; };

  # This actually fetches the sources for the declared third-party dependencies
  additional = lib.trivial.flip builtins.mapAttrs plan.additions
    (
      name: dep: pkgs.stdenv.mkDerivation {
        inherit name;
        inherit (dep) version;
        phases = "installPhase";
        installPhase = "ln -s $src $out";
        src = extraSources.${name} or
          (
            pkgs.fetchgit {
              rev = sha256map.${name}.rev;
              sha256 = sha256map.${name}.sha256;
              url = dep.repo;
            }
          );
      }
    );

  # The following two are necessary to prevent both Dhall and Spago from
  # attempting network connections. The idea is:
  #   - copy the installed Spago packages to a fake `XDG_CACHE_HOME` location
  #   - also copy a metadata file that Spago generates there
  #   - reconstruct a `packages.dhall` that does not contain any remote
  #     imports, by combining the raw Dhall additions along with the resolved
  #     upstream import (i.e. the downloaded Dhall file)
  prepareFakeSpagoEnv = ''
    export HOME="$TMP"
    mkdir -p "$HOME"/.cache/spago
    export XDG_CACHE_HOME="$HOME"/.cache
    cp -r ${cached}/* "$XDG_CACHE_HOME"/spago
    cp ${self}/lib/metadataV1.json "$XDG_CACHE_HOME"/spago
  '';
  fakePackagesDhall = pkgs.runCommand
    "fake-packages-dhall"
    {
      buildInputs = [ pkgs.dhall ];
    }
    ''
      cat <<EOF >additions.dhall
        ${plan.additions-dhall}
      EOF
      upstream=$(dhall --file ${../package-sets + "/${plan.upstream.path}/packages.dhall"})
      additions=$(dhall --file ./additions.dhall)
      dhall <<< "$upstream // $additions" > ./packages.dhall
      mkdir $out && mv ./packages.dhall $out
    '';

  # Unfortunately, we need both of these package sets, and there's a bit
  # of unavoidable waste between them
  #
  # `allPkgs` (and `cached` derived from it) contains all of the packages
  # in the upstream package set and the additional dependencies specified
  # in `packages.dhall`. It exists solely to trick `spago` into thinking
  # there's a global package cache in `XDG_CACHE_HOME`. If this isn't done,
  # `spago` will attempt to download things
  #
  # `spagoPkgs` is a subset of `allPkgs` and represents the actual project
  # dependencies (including transitive ones). `installed` can be "installed"
  # similar to how `spago install` works
  allPkgs = lib.attrsets.recursiveUpdate upstream additional;
  spagoPkgs = lib.attrsets.genAttrs lsDeps (dep: allPkgs.${dep});
  cached = utils.installOrCache "cache-spago-upstream" allPkgs;
  installed = utils.installOrCache "install-spago-deps" spagoPkgs;

  # This is a necessary step to get the exact project dependencies, including
  # the transitive dependencies. Otherwise, we will always build the entire
  # upstream package set during compilation
  lsDeps = import
    (
      pkgs.runCommand "${name}-ls-deps"
        {
          buildInputs = [ eps.spago pkgs.jq fakePackagesDhall ];
        }
        ''
          ${prepareFakeSpagoEnv}

          cp ${spagoDhall} ./spago.dhall
          cp ${fakePackagesDhall}/packages.dhall .
          mkdir $out
          spago ls deps -t --json |
            jq --slurp 'map(.packageName)' |
            tr ',' ' ' > $out/default.nix
        ''
    );

  # Spago doesn't provide a way to pass arguments to `psa` (which provides
  # for a nicer compilation experience than `purs` directly, so we want to
  # use it). We can shadow the real `psa` executable by wrapping it in a
  # shell script and intercepting the arguments passed to it from
  # `spago build` while also adding our own
  fakePsa =
    let
      psaArgs = builtins.concatStringsSep " "
        [
          (pkgs.lib.optionalString strict "--strict")
          (
            lib.optionalString
              (censorCodes != [ ])
              ''--censor-codes=${builtins.concatStringsSep "," censorCodes}''
          )
          "--censor-lib --is-lib=.spago"
        ];
    in
    pkgs.writeShellApplication {
      name = "psa";
      runtimeInputs = [ eps.psa ];
      text = ''
        psa ${psaArgs} "$@"
      '';
    };

  # Helper for derivations calling `spago build` or similar
  build =
    {
      # The command to run
      text
      # Any additional inputs to the derivation
    , extraInputs ? [ ]
      # Only used for naming the resulting derivation. Corresponds to the
      # `--deps-only` flag used by `spago-build`
    , depsOnly ? false
    , ...
    }: pkgs.runCommand ''${name}-output${lib.optionalString depsOnly "-deps"}''
      {
        nativeBuildInputs = [
          compiler
          fakePsa
          eps.spago
          fakePackagesDhall
          # `spago` invokes `git`
          pkgs.git
          pkgs.jq
        ] ++ extraInputs;
      }
      text;

  # Compile the project's dependencies. These can be cached and saved to copy
  # later
  outputDeps = build {
    depsOnly = true;
    # If any of the files in the `output` directory have a modified date later
    # then the dependencies (i.e. the contents of `.spago`), `purs` will
    # trigger a rebuild, hence the `find` commmand to set the date. The
    # modified date of all of the dependencies in `.spago` will be recorded
    # in the `cache-db.json` that `purs` generates; this will be copied later
    # in other derivations that build the project
    text = ''
      ${prepareFakeSpagoEnv}

      mkdir $out && cd $out
      cp ${fakePackagesDhall}/packages.dhall .
      cp ${spagoDhall} ./spago.dhall
      cp -r ${installed} .spago
      find .spago -exec touch -m {} +
      spago build --no-install --quiet
    '';
  };

  # Compile all of the project's dependencies and sources. We can use `spago`
  # by tricking it
  #
  # NOTE: This also copies all of the project sources into the resulting
  # derivation. `purs` doesn't provide a way to include any external files to its
  # `output` (and if we attempted to refer to absolute paths from the project-wide
  # `src` argument, they would be wrong)
  output = build {
    extraInputs = [ outputDeps ];
    # See the note on file modications times in `outputDeps` above. We can
    # make sure that `purs` doesn't rebuild by setting the modification date
    # of all files in `output` to the UNIX epoch
    text = ''
      ${prepareFakeSpagoEnv}

      mkdir $out && cd $out
      cp -r ${src}/* .
      chmod -R +rwx .
      cp ${fakePackagesDhall}/packages.dhall .
      ln -s ${installed} .spago
      mkdir output
      cp -r ${outputDeps}/output/* ./output
      find ./output -exec touch -m -d '01/01/1970' {} +
      chmod -R +rwx .
      spago build --no-install --quiet
    '';
  };


  # Bundle a module, corresponding to `spago bundle-module`
  bundle =
    { type
    , main ? "Main"
      # The file to write to, corresponds to `--to` flag
    , to ? "index.js"
    , ...
    }@args:
      assert lib.assertOneOf "bundle-type" type [ "app" "module" ];

      pkgs.runCommand ''${args.name or "${name}-bundle-${type}"}''
        {
          nativeBuildInputs = [ output compiler eps.spago pkgs.git ];
        }
        ''
          ${prepareFakeSpagoEnv}
          cp -r ${output}/* .
          cp -r ${installed} .spago
          chmod -R +rwx .

          mkdir $out
          spago bundle-${type} --no-build --no-install -m "${main}" \
            --to $out/${to}
        '';

  # Bundle a module, corresponding to `spago bundle-module`
  bundleModule =
    {
      # The main Purescript entrypoint
      main ? "Main"
      # The file to write to, corresponds to `--to` flag
    , to ? "index.js"
    , ...
    }@args: bundle ({ type = "module"; } // args);

  # Bundle an app, corresponding to `spago bundle-app`
  bundleApp =
    {
      # The main Purescript entrypoint
      main ? "Main"
      # The file to write to, corresponds to `--to` flag
    , to ? "index.js"
    , ...
    }@args:
    bundle ({ type = "app"; } // args);

  # Make a `devShell` from the options provided via `spagoProject.shell`,
  # all of which have default options
  mkDevShell =
    {
      # List of purescript development tools available from
      # `easy-purescript-nix`
      tools ? [ ]
      # Extra packages to include in the development environment
    , packages ? [ ]
      # If `true`, the Spago packages will be installed in `./.spago` in
      # the `devShell`'s `shellHook`
    , install ? true
    , shellHook ? ""
      # Generated `node_modules`. Can be explicitly passed to have better control
      # over individual project components
    , nodeModules ? projectNodeModules
    }:
    pkgs.mkShell {
      inherit packages;
      nativeBuildInputs = [
        eps.spago
        compiler
        nodejs
      ]
      # TODO handle different versions of tools, make `tools` a set?
      ++ builtins.map (tool: eps.${tool}) tools;
      shellHook =
        ''
          ${lib.optionalString packageLockOnly "export NPM_CONFIG_PACKAGE_LOCK_ONLY=true"}
          export NODE_PATH="${nodeModules}/lib/node_modules"
          export PATH="${nodeModules}/bin:$PATH"
        ''
        +
        (
          # NOTE
          # This ensures that `psci-support` is always in `installed`, otherwise
          # `spago` will download it
          let
            installed' = utils.installOrCache "install-spago-deps" (
              spagoPkgs // lib.attrsets.optionalAttrs
                (!(spagoPkgs ? psci-support))
                { inherit (allPkgs) psci-support; }
            );
          in
          lib.optionalString install
            ''
              if [[ -d .spago ]]; then
                rm -rf .spago
              fi
              mkdir .spago
              cp -r ${installed'}/* .spago
              chmod -R +rwx .spago
            ''
        )
        + shellHook;
    };
in
{
  # These can be generated for users
  flake = rec {
    devShells.default = mkDevShell shell;

    devShell = devShells.default;

    packages = {
      inherit output;
      nodeModules = projectNodeModules;
    };
  };

  # Because Spago offers no way to describing a project's structure and individual
  # components, we cannot generate these for users
  inherit bundleModule bundleApp;
}
