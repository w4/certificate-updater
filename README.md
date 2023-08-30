# certificate-updater

Quickly create and update certificates from a CA in Vault, automatically recreating them prior to expiry.

## Usage

Simply import the Nix Flake:

```nix
{
  inputs = {
    certificate-updater.url = "github:w4/certificate-updater";
  };

  outputs = {
    nixosConfigurations.my-host = {
      system = "x86_64-linux";
      modules = [
        certificate-updater.nixosModules."x86_64-linux".default
      ];
    };
  };
}
```

And enable the service:

```nix
{
  services.certificate-updater = {
    enable = true;
    role = "fortress-vector-agent"; # vault role to authenticate against
    commonName = "fortress.home"; # common name of the certificate to create
    ipAddress = "10.10.0.10"; # ip address to add as a SAN
    mount = "gaffken/v1/ica2/v1"; # mount point of the CA
    outputDirectory = "/var/lib/vector/certs"; # directory to write certificates out to
    environmentFile = config.age.secrets.cert-updater-env.path; # env file containing VAULT_TOKEN=...
    group = "vector"; # group that the application should run off, this group will also own the certs
  };
}
```
