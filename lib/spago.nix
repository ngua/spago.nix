{ pkgs, inputs, self, ... }:

let
  spagoProject = import ./project.nix {
    inherit self inputs pkgs;
  };
in
{ inherit spagoProject; }
