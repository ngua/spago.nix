{ inputs, self, ... }:
final: _:

let
  spago-nix = import ./lib/spago.nix {
    inherit self inputs;
    pkgs = final;
  };
in
{
  inherit spago-nix;
}
