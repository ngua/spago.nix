{ pkgs, ... }:
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
{
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
}
