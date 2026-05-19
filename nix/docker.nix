{ system }:
let
  pkgs = import ./. {
    inherit system;
    compiler = null;
  };
  feed-repeat = import ./release.nix {
    inherit system;
    static = true;
  };
in
pkgs.dockerTools.buildImage {
  name = "feed-repeat";
  tag = "latest";
  copyToRoot = [
    pkgs.dockerTools.caCertificates
    feed-repeat
  ];

  runAsRoot = ''
    #!${pkgs.runtimeShell}
    mkdir -p /var/lib/feed-repeat /var/cache/feed-repeat /etc/feed-repeat
  '';

  config = {
    Cmd = [
      "/bin/feed-repeat"
      "--config"
      "/etc/feed-repeat/config.yaml"
      "--output-dir"
      "/var/lib/feed-repeat"
      "--cache-dir"
      "/var/cache/feed-repeat"
    ];
    Volumes = {
      "/var/lib/feed-repeat" = { };
      "/var/cache/feed-repeat" = { };
      "/etc/feed-repeat" = { };
    };
  };
}
