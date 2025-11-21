{
  system ? builtins.currentSystem,
  compiler ? null,
}:
let
  pkgs = import ./nix { inherit system compiler; };
in
pkgs.mkShell {
  buildInputs = [ pkgs.feed-repeat.shell ];
  shellHook = ''
    export DEVSHELL_PATH="${pkgs.feed-repeat.shell}"
    export LD_LIBRARY_PATH="${pkgs.feed-repeat.shell}/lib$${LD_LIBRARY_PATH:+:$$LD_LIBRARY_PATH}"
    logo
  '';
}
