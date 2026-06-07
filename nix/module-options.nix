{ pkgs, serviceName }:
{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.services."${serviceName}";
  userName = cfg.userName;

  options = {
    enable = lib.mkEnableOption "${serviceName} service";

    package = lib.mkOption {
      type = lib.types.package;
      default = import ./release.nix {
        nixpkgs = pkgs.path;
        system = pkgs.system;
      };
      defaultText = "The feed-repeat Nix package provided in this repo.";
      description = "The feed-repeat package.";
    };

    config = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            sourceFeedUrl = lib.mkOption {
              type = lib.types.nonEmptyStr;
              description = "URL of the feed source";
              example = "https://example.com/feed.xml";
            };
            outputFilename = lib.mkOption {
              type = lib.types.nonEmptyStr;
              description = "Output filename prefix (without .atom extension)";
              example = "example-feed";
            };
            saveSourceFeedEntries = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Whether to cache the source feed locally";
            };
            repeatedEntryCount = lib.mkOption {
              type = lib.types.ints.unsigned;
              default = 3;
              description = "Number of entries to repeat in each run";
              example = 5;
            };
            minimumEntryAgeDays = lib.mkOption {
              type = lib.types.ints.unsigned;
              default = 7;
              description = "Minimum age in days for entries to be eligible for repetition";
              example = 3;
            };
            minRunGapDays = lib.mkOption {
              type = lib.types.ints.unsigned;
              default = 1;
              description = "Minimum gap in days between successive runs for this feed";
              example = 2;
            };
            maxEntryCountPerDomain = lib.mkOption {
              type = lib.types.nullOr lib.types.ints.positive;
              default = null;
              description = "Maximum number of entries to select from any single domain (optional)";
              example = 1;
            };
            selectionAlpha = lib.mkOption {
              type = lib.types.addCheck lib.types.float (x: x >= 0);
              default = 1.0;
              description = "Controls how strongly the weighted selection favors older entries. Higher values make older entries much more likely to be selected. Set to 0 for uniform random selection.";
              example = 1.5;
            };

            passthroughNewEntries = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Whether to pass through new entries (newer than the last output feed update) alongside repeated entries.";
            };
          };
        }
      );
      default = [ ];
      description = "List of feeds to process";
    };

    userName = lib.mkOption {
      type = lib.types.nonEmptyStr;
      default = "feed-repeat";
      description = "The username to use for running the feed-repeat service.";
    };

    outputDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/${userName}";
      description = "Directory to store output Atom files";
    };

    cacheDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/cache/${userName}";
      description = "Directory to cache source feed files";
    };

    timerOnCalendar = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "Systemd timer calendar expression for feed processing";
      example = "2days";
    };

    enableNginx = lib.mkEnableOption "Nginx for as a server for feed files";

    virtualHost = lib.mkOption {
      type = lib.types.nullOr lib.types.nonEmptyStr;
      default = null;
      description = ''
        The hostname of the feed-repeat server. This is
        used only if Nginx is enabled using the `enableNginx` option.
      '';
    };

    virtualHostPath = lib.mkOption {
      type = lib.types.nullOr lib.types.nonEmptyStr;
      default = null;
      description = ''
        The path component base URL of the feed-repeat server. This is
        used only if Nginx is enabled using the `enableNginx` option.
        Must end with a trailing slash (e.g. "/" or "/feeds/") to avoid
        nginx alias path-traversal pitfalls.
      '';
    };

    enableSSL = lib.mkEnableOption "SSL for Nginx";

    userAgent = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "User-Agent header to send in HTTP requests. Defaults to \"feed-repeat\" if not set.";
    };

    verbose = lib.mkEnableOption "verbose logging";

    quiet = lib.mkEnableOption "quiet logging (only warnings and errors)";
  };
in
{
  options.services.${serviceName} = options;
}
