{ self, inputs, pkgs, utils, ... }:
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
  #   - `package-lock.json`
  # These can be derived from the `src`, but can also be explicitly passed
, buildConfig ? { }
  # Various flags for compilation and installation:
  #   - `strict`; turns on `--strict` during compilation, e.g. `psa --strict ...`
  #   - `censorCodes`; list of warnings to silence during compilation
  #   - `development`; if set, the generated `node_modules` will also contain all
  #     `devDependencies`
, flags ? { }
  # If `true`, will build docs using default values for options. Even if this
  # is `false`, you can still use the `buildDocs` builder that is returned from
  # here
, withDocs ? true
, ...
}:
let
  inherit (pkgs) lib;

  packagesDhall = buildConfig.packagesDhall or /. + src + "/packages.dhall";
  spagoDhall = buildConfig.spagoDhall or /. + src + "/spago.dhall";
  packageJson = buildConfig.packageJson or /. + src + "/package.json";
  packageLock = buildConfig.packageLock or /. + src + "/package-lock.json";

  eps = import inputs.easy-purescript-nix { inherit pkgs; };

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

  nodeModules =
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
              node2nix ${lib.optionalString (flags.development or true) "--development" } \
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

  # Can be used to emulate `spago install` or to (mostly) create a mock global
  # package cache (`spago` always expects this)
  installOrCache = n: ps:
    let
      install = ps: pkgs.writeShellApplication {
        name = "install-spago-pkgs";
        runtimeInputs = [ ];
        text = ''
          set -e
          echo installing dependencies...
          ${builtins.toString (builtins.map cpPkg (builtins.attrValues ps))}
          echo 'done'
        '';
      };

      cpPkg = pkg:
        let
          target = "./${pkg.name}/${pkg.version}";
        in
        ''
          if [[ ! -e ${target} ]]; then
            echo "Installing ${target}."
            mkdir -p ${target}
            cp --no-preserve=mode,ownership,timestamp -r ${pkg}/* ${target}
          else
            echo "${target} already exists. Skipping."
          fi
        '';
    in
    pkgs.runCommand n
      {
        nativeBuildInputs = [ (install ps) ];
      }
      ''
        mkdir $out && cd $out
        install-spago-pkgs
      '';

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
  cached = installOrCache "cache-spago-upstream" allPkgs;
  installed = installOrCache "install-spago-deps" spagoPkgs;

  # This is a necessary step to get the exact project dependencies, including
  # the transitive dependencies. Otherwise, we will always build the entire
  # upstream package set during compilation
  lsDeps = import
    (
      pkgs.runCommandLocal "${name}-ls-deps"
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
          (lib.optionalString (flags.strict or true) "--strict")
          (
            let
              censorCodes = flags.censorCodes or [ ];
            in
            lib.optionalString (censorCodes != [ ])
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
    { text # The command to run
    , extraInputs ? [ ] # Any additional inputs to the derivation
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
    { type # `module` or `app`
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
    { main ? "Main" # The main Purescript entrypoint
    , to ? "index.js" # The file to write to, corresponds to `--to` flag
    , ...
    }@args: bundle ({ inherit main to; type = "module"; } // args);

  # Bundle an app, corresponding to `spago bundle-app`
  bundleApp =
    {
      # The main Purescript entrypoint
      main ? "Main"
      # The file to write to, corresponds to `--to` flag
    , to ? "index.js"
    , ...
    }@args:
    bundle ({ inherit main to; type = "app"; } // args);

  callNodeWithArgs = { output, arguments, command, main }:
    let
      sepSpace = builtins.concatStringsSep " ";
      arguments' =
        if arguments == [ ] then
          ''"$@"''
        else
          sepSpace arguments;
      nodeArgs = sepSpace [ command arguments' ];
    in
    ''node -e 'require("./output/${main}").main()' ${nodeArgs}'';

  nameFromMain = main: builtins.replaceStrings [ "." ] [ "-" ]
    (lib.strings.toLower main);

  # Run a Purescript application as a test with or without arguments
  runTest' =
    { testMain ? "Test.Main"
    , env ? { }
    , arguments ? [ ]
    , command ? ""
    , ...
    }@args:
    let
      modules = args.nodeModules or nodeModules;
    in
    pkgs.runCommand ''${args.name or "${name}-test-${nameFromMain testMain}"}''
      (
        {
          buildInputs = [ nodejs output ] ++ args.buildInputs or [ ];
          NODE_PATH = "${nodeModules}/lib/node_modules";
        }
        // env
      )
      # Since the project has already been built entirely, we can just use
      # Node directly to run the application
      ''
        cd ${output}
        ${callNodeWithArgs { inherit output arguments command; main = testMain; }}
        touch $out
      '';

  # Directly run a test corresponding to the provided Purescript main module
  runTest =
    { testMain ? "Test.Main" # The main Purescript entrypoint
    , env ? { } # Extra environment variables
    , ...
    }@args: runTest' ({ inherit testMain env; } // args);


  # Directly run a test corresponding to the provided Purescript main module,
  # with a command name and arguments
  #
  # NOTE: You must provide the command name as Node's `process.argv` includes
  # this as the first argument to the running application
  runTestWithArgs =
    { testMain ? "Test.Main" # The main Purescript entrypoint
    , env ? { } # Extra environment variables
    , command ? nameFromMain testMain
    , arguments ? [ ]
    , ...
    }@args:
      assert lib.asserts.assertMsg (command != "") "Command name cannot be empty";
      runTest' ({ inherit testMain env command arguments; } // args);

  # Create an executable from a Purescript main module and install it to the
  # given path
  nodeApp' =
    { main ? "Main"
    , env ? { }
    , arguments ? [ ]
    , command ? ""
    , ...
    }@args: pkgs.writeShellApplication {
      name = args.name or "${name}-app-${nameFromMain main}";
      runtimeInputs = [ nodejs ];
      text =
        let
          modules = args.nodeModules or nodeModules;
          exportEnv = builtins.concatStringsSep "\n"
            (lib.mapAttrsToList
              (
                k: v: "export ${k}=${builtins.toString v}"
              )
              env
            );
        in
        ''
          export NODE_PATH="${modules}/lib/node_modules"
          ${exportEnv}

          cd ${output}
          ${callNodeWithArgs { inherit output main arguments command; }}
        '';
    };

  # Create an executable from a Purescript main module and install it to the
  # given path.
  nodeApp =
    { main ? "Main" # The main Purescript entrypoint
      # Extra environment variables; will be `export`ed in the script.
      #
      # NOTE: You might wish to use `lib.strings.escapeShellArg` as this is not
      # done automatically for you
    , env ? { }
    , ...
    }@args: nodeApp' ({ inherit main env; } // args);

  # Directly run a test corresponding to the provided Purescript main module,
  # with a command name and arguments
  #
  # NOTE: You must provide the command name as Node's `process.argv` includes
  # this as the first argument to the invoked application
  nodeAppWithArgs =
    { main ? "Main" # The main Purescript entrypoint
      # Extra environment variables; will be `export`ed in the script.
      #
      # NOTE: You might wish to use `lib.strings.escapeShellArg` as this is not
      # done automatically for you
    , env ? { }
    , command ? nameFromMain main
    , arguments ? [ ]
    , ...
    }@args:
      assert lib.asserts.assertMsg (command != "")
        "Command name cannot be empty";
      nodeApp' ({ inherit main env command arguments; } // args);

  docsDeps = { format ? "html" }:
    assert lib.assertOneOf "format" format [ "html" "markdown" ];
    pkgs.runCommand
      "${name}-deps-docs"
      {
        buildInputs = [ compiler eps.spago pkgs.git nodejs ];
      }
      ''
        ${prepareFakeSpagoEnv}

        cp -r ${installed} .spago
        cp ${fakePackagesDhall}/packages.dhall .
        cp ${spagoDhall} ./spago.dhall
        chmod -R +rwx .
        find .spago -exec touch -m {} +

        spago docs --format ${format} --deps-only

        mkdir $out
        mv generated-docs $out
        mv output $out
      '';

  buildDocs =
    { format ? "html" # HTML or Markdown
      # If `true`, only build docs for the project dependencies
    , depsOnly ? false
    , ...
    }@args:
      assert lib.assertOneOf "format" format [ "html" "markdown" ];
      if depsOnly then
        docsDeps { inherit format; }
      else
        pkgs.runCommand
          (
            args.name or "${name}-docs"
          )
          {
            buildInputs = [
              compiler
              eps.spago
              pkgs.git
              nodejs
              (docsDeps { inherit format; })
            ];
          }
          ''
            ${prepareFakeSpagoEnv}

            cp -r ${installed} .spago
            cp -r ${src}/* .
            chmod -R +rwx .
            cp ${fakePackagesDhall}/packages.dhall .

            cp -r ${docsDeps { inherit format; }}/{generated-docs,output} .
            find ./output -exec touch -m -d '01/01/1970' {} +
            chmod -R +rwx .

            spago docs --format ${format}

            mkdir $out
            mv generated-docs $out
            mv output $out
          '';

  # Make a `devShell` from the options provided via `spagoProject.shell`,
  # all of which have default options
  mkDevShell =
    {
      # List of Purescript development tools available from
      # `easy-purescript-nix`
      tools ? [ ]
      # Extra packages to include in the development environment
    , packages ? [ ]
      # If `true`, the Spago packages will be installed in `./.spago` in
      # the `devShell`'s `shellHook`
    , install ? true
      # If `true`, `npm install` will only write to `package-lock.json`
    , packageLockOnly ? true
    , shellHook ? ""
    }@args:
    pkgs.mkShell {
      inherit packages;
      nativeBuildInputs = [
        eps.spago
        compiler
        nodejs
      ]
      ++ builtins.map (tool: eps.${tool}) tools;
      shellHook =
        let
          modules = args.nodeModules or nodeModules;
        in
        ''
          ${lib.optionalString packageLockOnly "export NPM_CONFIG_PACKAGE_LOCK_ONLY=true"}
          export NODE_PATH="${modules}/lib/node_modules"
          export PATH="${modules}/bin:$PATH"
        ''
        +
        (
          # NOTE
          # This ensures that `psci-support` is always in `installed`, otherwise
          # `spago` will download it
          let
            installed' = installOrCache "install-spago-deps" (
              spagoPkgs // lib.attrsets.optionalAttrs
                (!(spagoPkgs ? psci-support))
                { inherit (allPkgs) psci-support; }
            );
          in
          lib.optionalString install
            # A symlink will be faster, but Spago will always check if `.spago`
            # is writable when starting the REPL, hence the `cp -r` and `chmod`
            # invocations
            ''
              if [[ ! -d .spago ]]; then
                 mkdir .spago
              fi
              cp -r ${installed'}/* .spago
              chmod -R +rwx .spago
            ''
        )
        + shellHook;
    };
in
rec {
  # These can be generated for users
  flake = rec {
    devShells.default = mkDevShell shell;

    devShell = devShells.default;

    packages = {
      inherit output nodeModules;
    } // lib.attrsets.optionalAttrs withDocs {
      docs = buildDocs { };
    };
  } // lib.attrsets.optionalAttrs withDocs {
    apps.docs = utils.apps.fromDocs {
      inherit (flake.packages) docs;
    };
  };

  # Because Spago offers no way to describing a project's structure and individual
  # components, we cannot generate these for users. They are made available as
  # functions to generate derivations instead
  inherit
    bundleModule bundleApp
    runTest runTestWithArgs
    nodeApp nodeAppWithArgs
    buildDocs;
}
