{
  lib,
  config,
  pkgs,
  ...
}:
let
  serviceName = "feed-repeat";
  cfg = config.services.${serviceName};
  userName = cfg.userName;

  configFile = (pkgs.formats.yaml { }).generate "${serviceName}.yaml" cfg.config;
in
{
  imports = [ (import ./module-options.nix { inherit pkgs serviceName; }) ];

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
    users.users.${userName} = {
      isSystemUser = true;
      group = userName;
      createHome = false;
      home = cfg.outputDir;
    };
    users.users.${config.services.nginx.user} = lib.mkIf cfg.enableNginx {
      extraGroups = [ userName ];
    };
    users.groups.${userName} = { };

    systemd.tmpfiles.rules = [
      "d ${cfg.outputDir} 0750 ${userName} ${userName} -"
      "d ${cfg.cacheDir} 0750 ${userName} ${userName} -"
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
            ${lib.optionalString (cfg.userAgent != null) "--user-agent ${lib.escapeShellArg cfg.userAgent}"} \
            ${lib.optionalString cfg.verbose "--verbose"} \
            ${lib.optionalString cfg.quiet "--quiet"}
        '';
        User = userName;
        Group = userName;
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
      User = userName;
      Group = userName;
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
