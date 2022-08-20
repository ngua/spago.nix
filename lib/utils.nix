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
  install = spagoPkgs: pkgs.writeShellApplication
    {
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
}
