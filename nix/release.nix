{
  system ? builtins.currentSystem,
  compiler ? null,
  static ? false,
}:
let
  pkgs = import ./. {
    inherit compiler static;
    system = if static then "x86_64-linux" else system;
  };
  feed-repeat = pkgs.feed-repeat;
in
if static then feed-repeat.bin else feed-repeat
