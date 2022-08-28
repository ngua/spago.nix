{ pkgs, inputs, self, ... }:

let
  inherit (pkgs) lib;
  utils = import ./utils.nix { inherit pkgs; };
  spagoProject = import ./project.nix {
    inherit self inputs pkgs utils;
  };

in
{
  inherit spagoProject;
  utils = { inherit (utils) makeSha256map; };
}
