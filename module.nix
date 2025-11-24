{
  lib,
  config,
  pkgs,
  ...
}:
let
  serviceName = "feed-repeat";
  cfg = config.services.${serviceName};
  feedRepeatPkg = import ./nix/release.nix { static = true; };

  configFile = (pkgs.formats.yaml { }).generate "${serviceName}.yaml" cfg.config;
in
{
  options.services.feed-repeat = {
    enable = lib.mkEnableOption "feed-repeat service";

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
            cacheSourceFeed = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Whether to cache the source feed locally";
            };
            repeatedEntryCount = lib.mkOption {
              type = lib.types.int;
              default = 3;
              description = "Number of entries to repeat in each run";
              example = 5;
            };
            minimumEntryAgeDays = lib.mkOption {
              type = lib.types.int;
              default = 7;
              description = "Minimum age in days for entries to be eligible for repetition";
              example = 3;
            };
            minRunGapDays = lib.mkOption {
              type = lib.types.int;
              default = 1;
              description = "Minimum gap in days between successive runs for this feed";
              example = 2;
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
      '';
    };

    enableSSL = lib.mkEnableOption "SSL for Nginx";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion =
          if cfg.enableNginx then cfg.virtualHost != null && cfg.virtualHostPath != null else true;
        message = "Nginx is enabled but the virtualHost and/or virtualHostPath options are not set.";
      }
    ];
    users.users.${serviceName} = {
      isSystemUser = true;
      group = serviceName;
      createHome = false;
      home = cfg.outputDir;
    };
    users.users.${config.services.nginx.user} = lib.mkIf cfg.enableNginx {
      extraGroups = [serviceName];
    };
    users.groups.${serviceName} = { };

    systemd.tmpfiles.rules = [
      "d ${cfg.outputDir} 0750 ${serviceName} ${serviceName} -"
      "d ${cfg.cacheDir} 0750 ${serviceName} ${serviceName} -"
    ];

    systemd.services.${serviceName} = {
      enable = true;
      description = "${serviceName} service";
      startAt = cfg.timerOnCalendar;
      restartIfChanged = true;
      restartTriggers = [
        feedRepeatPkg
        configFile
      ];
      serviceConfig = {
        ExecStart = "${feedRepeatPkg}/bin/feed-repeat --config ${configFile} --output-dir ${cfg.outputDir} --cache-dir ${cfg.cacheDir}";
        User = serviceName;
        Group = serviceName;
        Type = "oneshot";
        WorkingDirectory = cfg.outputDir;
        Restart = "on-failure";
        RestartSec = "5s";

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
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateMounts = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "full";
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        SystemCallFilter = [
          "~@clock"
          "~@debug"
          "~@module"
          "~@mount"
          "~@obsolete"
          "~@reboot"
          "~@setuid"
          "~@swap"
        ];
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
          };
        };
      };
    };
  };
}
