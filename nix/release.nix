{
  sources ? import ./sources.nix,
  nixpkgs ? sources.nixpkgs,
  system ? builtins.currentSystem,
  compiler ? null,
  static ? false,
}:
let
  pkgs = import ./. {
    inherit
      sources
      nixpkgs
      compiler
      static
      system
      ;
  };
  feed-repeat = pkgs.feed-repeat;
in
if static then feed-repeat.bin else feed-repeat
