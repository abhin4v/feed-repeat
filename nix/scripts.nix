{ pkgs }:
let
  logo = pkgs.writeShellScriptBin "logo" ''
    set -euo pipefail
    echo -e "\n$(tput setaf 2)"
    echo feed-repeat | ${pkgs.figlet}/bin/figlet
    echo -e "$(tput sgr0)\n"
  '';
  build = pkgs.writeShellScriptBin "build" ''
    set -euo pipefail
    nix-build nix/release.nix
  '';
  build-static = pkgs.writeShellScriptBin "build-static" ''
    set -euo pipefail
    [[ $# -eq 1 ]] || { echo "Usage: build-static <arch>" >&2; exit 1; }
    nix-build nix/release.nix --arg static true --argstr system "$1-linux"
    # add Nix GC root for static dependencies and build tools
    nix-store --add-root .gcroots/static-deps-$1 \
      --realise `nix-instantiate --quiet --quiet --quiet nix/static-deps.nix` \
      > /dev/null
  '';
  run = pkgs.writeShellScriptBin "run" ''
    set -euo pipefail
    result/bin/feed-repeat --config config.yaml --output-dir output --cache-dir cache | awk '{ print strftime("%Y-%m-%d %H:%M:%S"), $0; }'
  '';
  build-docker = pkgs.writeShellScriptBin "build-docker" ''
    set -euo pipefail
    [[ $# -eq 1 ]] || { echo "Usage: build-docker <arch>" >&2; exit 1; }
    nix-build nix/docker.nix --argstr system "$1-linux"
  '';
in
[
  logo
  build
  build-static
  build-docker
  run
]
