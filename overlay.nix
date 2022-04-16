{ inputs }:
_: prev:
let
  easy-ps = import inputs.easy-purescript-nix { pkgs = prev; };
in
{
  purescript-nix = {
    purescriptProject =
      { compiler ? "0.14.5"
      , shell ? { }
      , ...
      }:
        assert prev.lib.assertOneOf "compiler" compiler [ "0.14.5" ];
        let
          compilerName = builtins.replaceStrings [ "." ] [ "_" ] compiler;

          purs = easy-ps."purs-${compilerName}";

          # Make a `devShell` from the options provided via `purescriptProject.shell`,
          # all of which have default options
          mkDevShell =
            {
              # List of purescript development tools available from
              # easy-purescript-nix
              tools ? [ ]
              # Extra packages to include in the development environment
            , buildInputs ? [ ]
            }: prev.mkShell {
              buildInputs = [ purs easy-ps.spago ]
                ++ buildInputs
                ++ builtins.map
                (tool:
                  assert prev.lib.assertOneOf "tool" tool [
                    "purty"
                    "psa"
                    "pscid"
                    "purescript-language-server"
                    "purs-tidy"
                  ];
                  easy-ps."${tool}"
                )
                tools;
            };
        in
        rec {
          devShell = mkDevShell shell;

          flake = { inherit devShell; };
        };
  };
}
