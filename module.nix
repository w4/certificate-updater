{ config, lib, pkg, ... }:
with lib;
let
  cfg = config.services.certificate-updater;
in
{
  options.services.certificate-updater = {
    enable = mkEnableOption "certificate-updater";

    role = mkOption {
      description = "Vault role to create certificate for";
      type = types.str;
    };

    commonName = mkOption {
      description = "Common name to create the certificate for";
      type = types.str;
    };

    ipAddress = mkOption {
      description = "IP Address to include in SAN";
      type = types.str;
    };

    outputDirectory = mkOption {
      description = "Directory to write certificates out to";
      type = types.path;
    };

    mount = mkOption {
      description = "Mount point of the CA to create certificate from";
      type = types.str;
    };

    environmentFile = mkOption {
      description = "Path to environment file containing VAULT_TOKEN";
      type = types.path;
    };

    group = mkOption {
      description = "Group to write files as";
      type = types.str;
    };

    host = mkOption {
      default = "http://vault.home";
      description = "URL to access Vault";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    systemd.timers.certificate-updater = {
      enable = true;
      wantedBy = [ "timers.target" ];
      after = [ "network-online.target" ];
      timerConfig = {
        OnBootSec = "0s";
        OnCalendar = "*:0/5";
        RandomizedDelaySec = "120";
        Unit = "certificate-updater.service";
      };
    };

    systemd.services.certificate-updater = {
      serviceConfig = {
        Type = "oneshot";
        EnvironmentFile = "${cfg.environmentFile}";
        ExecStart = "${pkg}/bin/certificate-updater -r \"${cfg.role}\" -c \"${cfg.commonName}\" -i \"${cfg.ipAddress}\" -o \"${cfg.outputDirectory}\" -u \"${cfg.host}\" -m \"${cfg.mount}\"";
        Group = cfg.group;
      };
    };
  };
}
