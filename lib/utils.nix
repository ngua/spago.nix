{ pkgs, ... }:

let
  cpPackage = pkg:
    let
      target = ".spago/${pkg.name}/${pkg.version}";
    in
    ''
      if [ ! -e ${target} ]; then
        echo "Installing ${target}."
        mkdir -p ${target}
        cp --no-preserve=mode,ownership,timestamp -r \
           ${toString pkg.outPath}/* ${target}
      else
        echo "${target} already exists. Skipping."
      fi
    '';

  getGlob = pkg: ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';
in
{
  install = spagoPkgs: pkgs.writeShellApplication {
    name = "install-spago-pkgs";
    runtimeInputs = [ ];
    text = ''
      set -e
      echo installing dependencies...
      ${builtins.toString
        (builtins.map cpPackage (builtins.attrValues spagoPkgs))
       }
      echo "echo done."
    '';
  };

  # Turns flake `inputs` into a `sha256map` that can be used with `spagoProject`,
  # assuming that the extra dependencies are pinned using a flake
  makeSha256map = builtins.mapAttrs (_: v: {
    inherit (v) rev;
    sha256 = v.narHash;
  });
}
