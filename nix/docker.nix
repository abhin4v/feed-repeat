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
pkgs.dockerTools.buildLayeredImage {
  name = "feed-repeat";
  tag = "latest";
  contents = [
    pkgs.dockerTools.caCertificates
    feed-repeat
  ];

  extraCommands = ''
    mkdir -p var/lib/feed-repeat var/cache/feed-repeat etc/feed-repeat
    echo 'feed-repeat:x:1000:1000::/var/lib/feed-repeat:/sbin/nologin' > etc/passwd
    echo 'feed-repeat:x:1000:' > etc/group
  '';
  fakeRootCommands = ''
    chown -R 1000:1000 var/lib/feed-repeat var/cache/feed-repeat
  '';
  enableFakechroot = true;

  config = {
    User = "1000:1000";
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
