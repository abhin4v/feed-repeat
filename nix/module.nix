{
  lib,
  config,
  pkgs,
  ...
}:
let
  serviceName = "feed-repeat";
  cfg = config.services.${serviceName};

  configFile = (pkgs.formats.yaml { }).generate "${serviceName}.yaml" cfg.config;
in
{
  options.services.feed-repeat = {
    enable = lib.mkEnableOption "feed-repeat service";

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

    outputDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/lib/feed-repeat";
      description = "Directory to store output Atom files";
    };

    cacheDir = lib.mkOption {
      type = lib.types.path;
      default = "/var/cache/feed-repeat";
      description = "Directory to cache source feed files";
    };

    timerOnCalendar = lib.mkOption {
      type = lib.types.str;
      default = "daily";
      description = "Systemd timer calendar expression for feed processing";
      example = "2days";
    };

    enableNginx = lib.mkEnableOption ''Nginx for as a server for feed files'';

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

    verbose = lib.mkEnableOption "verbose logging";

    quiet = lib.mkEnableOption "quiet logging (only warnings and errors)";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          if cfg.enableNginx then cfg.virtualHost != null && cfg.virtualHostPath != null else true;
        message = "Nginx is enabled but the virtualHost and/or virtualHostPath options are not set.";
      }
      {
        assertion =
          if cfg.enableNginx && cfg.virtualHostPath != null then
            lib.hasSuffix "/" cfg.virtualHostPath
          else
            true;
        message = ''
          services.feed-repeat.virtualHostPath ("${toString cfg.virtualHostPath}") must end
          with a trailing slash (e.g. "/" or "/feeds/").
        '';
      }
      {
        assertion = !(cfg.verbose && cfg.quiet);
        message = "Both verbose and quiet options are enabled at the same time.";
      }
    ];
    users.users.${serviceName} = {
      isSystemUser = true;
      group = serviceName;
      createHome = false;
      home = cfg.outputDir;
    };
    users.users.${config.services.nginx.user} = lib.mkIf cfg.enableNginx {
      extraGroups = [ serviceName ];
    };
    users.groups.${serviceName} = { };

    systemd.tmpfiles.rules = [
      "d ${cfg.outputDir} 0750 ${serviceName} ${serviceName} -"
      "d ${cfg.cacheDir} 0750 ${serviceName} ${serviceName} -"
    ];

    systemd.services.${serviceName} = {
      enable = true;
      description = "${serviceName} service";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      startAt = cfg.timerOnCalendar;
      restartIfChanged = true;
      restartTriggers = [
        cfg.package
        configFile
      ];
      environment = {
        RUNNING_UNDER_SYSTEMD = "1";
      };
      serviceConfig = {
        ExecStart = ''
          ${cfg.package}/bin/feed-repeat \
            --config ${configFile} \
            --output-dir ${cfg.outputDir} \
            --cache-dir ${cfg.cacheDir} \
            ${lib.optionalString cfg.verbose "--verbose"} \
            ${lib.optionalString cfg.quiet "--quiet"}
        '';
        User = serviceName;
        Group = serviceName;
        Type = "oneshot";
        WorkingDirectory = cfg.outputDir;

        AmbientCapabilities = [ ];
        CapabilityBoundingSet = [
          "~CAP_RAWIO"
          "~CAP_MKNOD"
          "~CAP_AUDIT_CONTROL"
          "~CAP_AUDIT_READ"
          "~CAP_AUDIT_WRITE"
          "~CAP_SYS_BOOT"
          "~CAP_SYS_TIME"
          "~CAP_SYS_MODULE"
          "~CAP_SYS_PACCT"
          "~CAP_LEASE"
          "~CAP_LINUX_IMMUTABLE"
          "~CAP_IPC_LOCK"
          "~CAP_BLOCK_SUSPEND"
          "~CAP_WAKE_ALARM"
          "~CAP_SYS_TTY_CONFIG"
          "~CAP_MAC_ADMIN"
          "~CAP_MAC_OVERRIDE"
          "~CAP_NET_ADMIN"
          "~CAP_NET_BROADCAST"
          "~CAP_NET_RAW"
          "~CAP_SYS_ADMIN"
          "~CAP_SYS_PTRACE"
          "~CAP_SYSLOG"
        ];
        DevicePolicy = "closed";
        KeyringMode = "private";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateMounts = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProcSubset = "pid";
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectSystem = "strict";
        ReadWritePaths = [
          cfg.outputDir
          cfg.cacheDir
        ];
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
          "~@resources"
        ];
        UMask = "0027";
      };
    };

    systemd.timers."${serviceName}".timerConfig = {
      User = serviceName;
      Group = serviceName;
    };

    services.nginx = lib.mkIf cfg.enableNginx {
      enable = true;
      virtualHosts."${cfg.virtualHost}" = {
        forceSSL = cfg.enableSSL;
        enableACME = cfg.enableSSL;
        locations = {
          "${cfg.virtualHostPath}" = {
            alias = "${cfg.outputDir}/";
            extraConfig = ''
              types {
                application/atom+xml atom;
              }
              default_type application/octet-stream;
              autoindex off;
              expires 6h;
              add_header Cache-Control "public, max-age=21600";
              add_header Strict-Transport-Security "max-age=31536000" always;
              add_header X-Content-Type-Options "nosniff" always;

              # Deny access to dotfiles (e.g. .git, .htaccess) if any ever appear
              # in the output directory.
              location ~ /\. {
                deny all;
                return 404;
              }
            '';
          };
        };
      };
    };
  };
}
