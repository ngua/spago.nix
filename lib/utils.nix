{ pkgs, ... }:

let
  mkTarget = pkg: "./${pkg.name}/${pkg.version}";

  cpPackage = pkg:
    let
      target = mkTarget pkg;
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
in
rec {
  getSpagoGlob = pkg: ''".spago/${mkTarget pkg}/src/**/*.purs"'';

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

  # Can be used to emulate `spago install` or to (mostly) create a mock global
  # package cache (`spago` always expects this)
  installOrCache = name: ps: pkgs.runCommand
    name
    {
      nativeBuildInputs = [ (install ps) ];
    }
    ''
      mkdir $out && cd $out
      install-spago-pkgs
    '';

  # Turns flake `inputs` into a `sha256map` that can be used with `spagoProject`,
  # assuming that the extra dependencies are pinned using a flake
  makeSha256map = builtins.mapAttrs (_: v: {
    inherit (v) rev;
    sha256 = v.narHash;
  });
}
