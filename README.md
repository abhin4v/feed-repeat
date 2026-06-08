# feed-repeat

A Haskell tool that repeats entries from RSS/Atom feeds into new feeds. It fetches entries from source feeds, and selects a random subset using weighted sampling where older entries have higher priority, and inserts them in output feeds. [This blog post](https://abhinavsarkar.net/notes/2026-feed-repeat/) describes the motivation behind it.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Prerequisites](#prerequisites)
- [Building](#building)
  - [Build with Cabal](#build-with-cabal)
  - [Build with Nix](#build-with-nix)
- [Usage](#usage)
  - [NixOS Module](#using-as-a-nixos-module)
  - [systemd Service](#using-as-a-systemd-service)
  - [Docker](#using-as-a-docker-image)
  - [Web Server](#serving-feeds-with-a-web-server)
  - [GitHub Pages](#hosting-on-github-pages)
- [CLI Usage](#cli-usage)
- [Configuration](#configuration)
- [License](#license)
- [Changelog](#changelog)
- [Contributing](#contributing)

## Features

- Processes multiple source feeds with individual configurations.
- Uses exponential weighting to prioritize older entries.
- Caches fetched feeds to handle source feed unavailability.
- Filters entries by minimum age to avoid repeating recent content.
- Supports RSS, Atom and RDF feed formats.

## Installation

`feed-repeat` is available as statically-linked binaries for AArch64 and AMD64 architectures in the releases. It is also available as a [Docker image](https://github.com/abhin4v/feed-repeat/pkgs/container/feed-repeat) in the GitHub Container Repo.

## Prerequisites

This project is written in [Haskell](https://www.haskell.org/). You don't need Haskell experience to use this tool, but you'll need the Haskell compiler and build tools installed to build it.

The easiest way to install Haskell is via [GHCup](https://www.haskell.org/ghcup/). Run GHCup to install GHC (9.10+) and Cabal (3.4+). Alternatively, check your system's package manager for pre-built packages.

[Nix](https://nixos.org) is optional. It is required for `nix` builds and NixOS module support.

## Building

First, clone the repository and navigate into it:

```bash
git clone https://github.com/abhin4v/feed-repeat.git
cd feed-repeat
```

### Build with Cabal

```bash
cabal build
```

### Build with Nix

Enter the Nix shell:

```bash
nix-shell
```

Run the scripts available in Nix shell:

```bash
# Build the project
build

# Build a static binary
build-static x86_64
# or build-static aarch64

# Run the tool with example config
run
```

## Usage

This project can be used as a Nix module, a Systemd service, a Docker container, or hosted on GitHub Pages.

### Using as a NixOS Module

The project includes a NixOS module (`nix/module.nix`) for easy integration into NixOS systems. Import it in your configuration:

```nix
{
  imports = [ ./feed-repeat/nix/module.nix ];

  services.feed-repeat = {
    enable = true;
    
    # Feed configurations
    config = [
      {
        sourceFeedUrl = "https://example.com/feed.atom";
        outputFilename = "example-feed";
        saveSourceFeedEntries = true;
        repeatedEntryCount = 3;
        minimumEntryAgeDays = 7;
        maxEntryCountPerDomain = 1;
        selectionAlpha = 0.9;
      }
    ];
    
    # Output and cache directories
    outputDir = "/var/lib/feed-repeat";
    cacheDir = "/var/cache/feed-repeat";
    
    # Run frequency
    timerOnCalendar = "daily";
    
    # Optional: serve feeds via Nginx
    enableNginx = true;
    virtualHost = "feeds.example.com";
    virtualHostPath = "/";
    enableSSL = true;
  };
}
```

The module automatically:

- Creates a systemd service with configurable timer.
- Sets up user/group with appropriate permissions.
- Generates the configuration file from your NixOS settings.
- Optionally configures Nginx to serve the output feeds.

### Using as a systemd Service

For non-NixOS systems, a systemd service file (`configs/feed-repeat.service`) is provided. To set it up:

1. Create user and group:

    ```bash
    sudo useradd -r -s /bin/false feed-repeat
    ```

2. Create required directories:

    ```bash
    sudo mkdir -p /var/lib/feed-repeat /var/cache/feed-repeat /etc/feed-repeat
    sudo chown feed-repeat:feed-repeat /var/lib/feed-repeat /var/cache/feed-repeat
    sudo chmod 750 /var/lib/feed-repeat /var/cache/feed-repeat
    ```

3. Add web server user to feed-repeat group:

    ```bash
    sudo usermod -a -G feed-repeat www-data
    ```
    This allows the web server (running as www-data) to read the output feeds from `/var/lib/feed-repeat`. Change the user as appropriate.

4. Install the service file:

    ```bash
    sudo cp configs/feed-repeat.service /etc/systemd/system/
    ```

5. Place your configuration:

    ```bash
    sudo cp config.yaml /etc/feed-repeat/config.yaml
    sudo chown feed-repeat:feed-repeat /etc/feed-repeat/config.yaml
    sudo chmod 640 /etc/feed-repeat/config.yaml
    ```

6. Build and install the binary:

    ```bash
    cabal install --installdir=/tmp --install-method=copy --overwrite-policy=always
    sudo install -D -m 0755 /tmp/feed-repeat /usr/local/bin/feed-repeat
    ```
    Or use the binaries available for download.

7. Install the timer unit:

    ```bash
    sudo cp configs/feed-repeat.timer /etc/systemd/system/
    ```

8. Enable and start the service:

    ```bash
   sudo systemctl daemon-reload
   sudo systemctl enable --now feed-repeat.timer
   ```

### Using as a Docker Image

A Docker image can be built with Nix:

```bash
# Enter nix-shell, then build the Docker image
build-docker x86_64
# or build-docker aarch64

# Load into Docker daemon
docker load < result

# Alternatively, you can pull the pre-built image from GHCR
docker pull ghcr.io/abhin4v/feed-repeat:latest

# Run the container
docker run --rm \
  -v /path/to/config.yaml:/etc/feed-repeat/config.yaml:ro \
  -v feed-repeat-output:/var/lib/feed-repeat \
  -v feed-repeat-cache:/var/cache/feed-repeat \
  feed-repeat:latest
```

The container runs as a non-root user (UID/GID `1000:1000`). If you bind-mount
host directories instead of using named volumes, ensure they are writable by
that UID, for example:

```bash
sudo chown -R 1000:1000 /path/to/output /path/to/cache
```

Named Docker volumes (as used in the examples above) are handled
automatically by the Docker runtime.

#### Scheduling Runs

Since the container runs once and exits, you need to schedule it externally:

- Use the host's cron or systemd timer to run the container periodically:

    ```bash
    # Via cron: add to crontab (runs daily at 2 AM)
    0 2 * * * docker run -v /path/to/config.yaml:/etc/feed-repeat/config.yaml:ro -v feed-repeat-output:/var/lib/feed-repeat -v feed-repeat-cache:/var/cache/feed-repeat feed-repeat:latest
    ```

- Docker Compose with Ofelia: Use Docker Compose with the Ofelia scheduler to run the container on a schedule:

    ```yaml
    services:
      feed-repeat:
        image: feed-repeat:latest
        volumes:
          - /path/to/config.yaml:/etc/feed-repeat/config.yaml:ro
          - feed-repeat-output:/var/lib/feed-repeat
          - feed-repeat-cache:/var/cache/feed-repeat
        labels:
          ofelia: "enabled"
          ofelia.enabled: "true"
          ofelia.my-task.schedule: "@daily"
          ofelia.my-task.command: "/bin/feed-repeat --config /etc/feed-repeat/config.yaml --output-dir /var/lib/feed-repeat --cache-dir /var/cache/feed-repeat"
    
      ofelia:
        image: mcuadros/ofelia:latest
        volumes:
          - /var/run/docker.sock:/var/run/docker.sock
        command: daemon --docker
    
    volumes:
      feed-repeat-output:
      feed-repeat-cache:
    ```
    
    Run with: `docker-compose up -d`.

- Kubernetes: If deployed on Kubernetes, use native `CronJob` resources for scheduling.
- Docker Swarm: Use native scheduled task features if using Docker Swarm.

### Serving Feeds with a Web Server

To serve the output feeds publicly, you can use any web server. Basic example configurations are provided for Nginx, Apache, and Caddy in the `configs` directory.

### Hosting on GitHub Pages

You can run and host `feed-repeat` on GitHub Actions and Pages: fork this repo, edit `config.yaml`, and let GitHub Actions publish your repeated feeds to GitHub Pages. See the full [Hosting on GitHub Pages guide](https://abhin4v.github.io/feed-repeat/hosting-on-github-pages.html) for the step-by-step setup.

## CLI Usage

```bash
feed-repeat --config config.yaml --output-dir ./output --cache-dir ./cache
```

### Options

- `--config FILE`: Path to YAML configuration file containing feed sources (required).
- `--output-dir DIR`: Directory where output Atom files will be written (required).
- `--cache-dir DIR`: Directory where cached Atom files will be stored (default: current directory).
- `--user-agent STRING`: User-Agent header to send in HTTP requests (default: 'feed-repeat').
- `--validate`: Only validate the config file and exit.
- `--verbose`: Enable all logging.
- `--quiet`: Enable only warning and error logging.
- `--version`: Show version information.

## Configuration

Create a YAML file with a list of feed tasks:

```yaml
- sourceFeedUrl: "https://example.com/feed.atom"
  outputFilename: "unique-id-1"
  saveSourceFeedEntries: true
  repeatedEntryCount: 3
  minimumEntryAgeDays: 7
  maxEntryCountPerDomain: 1
  selectionAlpha: 0.9
  passthroughNewEntries: true

- sourceFeedUrl: "https://another-site.com/rss.xml"
  outputFilename: "unique-id-2"
  saveSourceFeedEntries: false
  repeatedEntryCount: 1
  minimumEntryAgeDays: 14
```

See `config.yaml` for all available parameters and their meanings.

## License

MIT

## Changelog

See [CHANGELOG](CHANGELOG.md).

## Contributing

I consider this is a done software. Maybe some day when [JSONFeed](https://www.jsonfeed.org/) gets popular, I'd consider adding support for it. Other than that, I don't foresee adding any new features. I'll keep doing bugfixes, security fixes and dependency upgrades.

Please feel free to create an issue if you find a bug. I'm not inclined to accept pull requests unless there is a very compelling reason.

Disclaimer: This is a personal project. The views, code, and opinions expressed here are my own and do not represent those of my current or past employers.
