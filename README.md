# feed-repeat

A Haskell tool that repeats entries from RSS/Atom feeds into new feeds. It fetches entries from source feeds, filters them by age, and selects a random subset for inclusion in output feeds using weighted sampling where older entries have higher priority.

## Features

- **Multi-feed support**: Process multiple source feeds with individual configurations.
- **Intelligent entry selection**: Uses exponential weighting to prioritize older entries.
- **Caching**: Optionally cache fetched feeds to handle source feed unavailability.
- **Filtering**: Filter entries by minimum age to avoid repeating recent content.
- **Format conversion**: Automatically converts RSS/RDF feeds to Atom format.

## About & Prerequisites

This project is written in [Haskell](https://www.haskell.org/), a statically-typed functional programming language. You don't need Haskell experience to use this tool, but you'll need the Haskell compiler and build tools installed.

### Installing Haskell

The easiest way to install Haskell is via [GHCup](https://www.haskell.org/ghcup/):

```bash
# Install GHCup (follow the prompts)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Run GHCup to install GHC (the compiler) and Cabal (the build tool). Alternatively, check your system's package manager (e.g., `apt`, `brew`, `pacman`) for pre-built packages.

### Building Prerequisites

- **GHC 9.10+** (Haskell compiler)
- **Cabal 3.4+** (Build tool)
- **Nix** (optional, for `nix` builds and NixOS module support)

Verify your installation:

```bash
ghc --version
cabal --version
```

## Building

First, clone the repository and navigate into it:

```bash
git clone https://code.abhinavsarkar.net/abhin4v/feed-repeat.git
cd feed-repeat
```

### Build with Cabal

```bash
cabal build
```

### Build with Nix

Enter the Nix development environment:

```bash
nix-shell
```

Available scripts in Nix shell (defined in `scripts.nix`):

```bash
# Build the project
build

# Build a static binary
build-static

# Run the tool with example config
run
```

## Using as a NixOS Module

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
        cacheSourceFeed = true;
        repeatedEntryCount = 3;
        minimumEntryAgeDays = 7;
      }
    ];
    
    # Output and cache directories (defaults shown)
    outputDir = "/var/lib/feed-repeat";
    cacheDir = "/var/cache/feed-repeat";
    
    # Run frequency (systemd calendar expression, default: daily)
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
- Applies strict security hardening to the service.

## Using as a systemd Service

For non-NixOS systems, a systemd service file (`configs/feed-repeat.service`) is provided. To set it up:

1. **Create user and group**:
    ```bash
    sudo useradd -r -s /bin/false feed-repeat
    ```

2. **Create required directories**:
    ```bash
    sudo mkdir -p /var/lib/feed-repeat /var/cache/feed-repeat /etc/feed-repeat
    sudo chown feed-repeat:feed-repeat /var/lib/feed-repeat /var/cache/feed-repeat
    sudo chmod 750 /var/lib/feed-repeat /var/cache/feed-repeat
    ```

3. **Add web server user to feed-repeat group**:
    ```bash
    sudo usermod -a -G feed-repeat www-data
    ```
    This allows the web server (running as www-data) to read the output feeds from `/var/lib/feed-repeat`. Change the user as appropriate.

4. **Install the service file**:
    ```bash
    sudo cp configs/feed-repeat.service /etc/systemd/system/
    ```

5. **Place your configuration**:
    ```bash
    sudo cp config.yaml /etc/feed-repeat/config.yaml
    sudo chown feed-repeat:feed-repeat /etc/feed-repeat/config.yaml
    sudo chmod 640 /etc/feed-repeat/config.yaml
    ```

6. **Build and install the binary**:
     ```bash
     cabal install --installdir=/tmp --install-method=copy --overwrite-policy=always
     sudo install -D -m 0755 /tmp/feed-repeat /usr/local/bin/feed-repeat
     ```

7. **Install the timer unit**:
    ```bash
    sudo cp configs/feed-repeat.timer /etc/systemd/system/
    ```

8. **Enable and start the service**:
   ```bash
   sudo systemctl daemon-reload
   sudo systemctl enable --now feed-repeat.timer
   ```

## Using as a Docker Image

A Docker image can be built with Nix:

```bash
# Enter nix-shell, then build the Docker image
build-docker

# Load into Docker daemon
docker load < result

# Run the container
docker run \
  -v /path/to/config.yaml:/etc/feed-repeat/config.yaml:ro \
  -v feed-repeat-output:/var/lib/feed-repeat \
  -v feed-repeat-cache:/var/cache/feed-repeat \
  feed-repeat:latest
```

The Docker image includes:
- Static binary built for x86_64-linux
- CA certificates for HTTPS feed fetching
- Mount points for configuration, output, and cache directories

### Scheduling Runs

Since the container runs once and exits, you need to schedule it externally:

1. **Host-level cron/systemd** (recommended): Use the host's cron or systemd timer to run the container periodically:
    ```bash
    # Via cron: add to crontab (runs daily at 2 AM)
    0 2 * * * docker run -v /path/to/config.yaml:/etc/feed-repeat/config.yaml:ro -v feed-repeat-output:/var/lib/feed-repeat -v feed-repeat-cache:/var/cache/feed-repeat feed-repeat:latest
    ```

2. **Docker Compose with Ofelia**: Use Docker Compose with the Ofelia scheduler to run the container on a schedule:
    ```yaml
    version: '3.8'
    
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
    
    Run with: `docker-compose up -d`

3. **Kubernetes**: If deployed on Kubernetes, use native `CronJob` resources for scheduling.

4. **Docker Swarm**: Use native scheduled task features if using Docker Swarm.

## Serving Feeds with a Web Server

To serve the output feeds publicly, you can use any web server. Example configurations are provided for:

- **Nginx**: `configs/nginx.conf.example`
- **Apache**: `configs/apache.conf.example`
- **Caddy**: `configs/Caddyfile.example`

All examples include:
- Automatic HTTPS with Let's Encrypt
- Security headers (HSTS, X-Content-Type-Options, etc.)
- Proper Atom feed content-type handling
- 6-hour caching for feed files

Choose the configuration that matches your web server and customize the domain name and paths as needed.

## Usage

```bash
feed-repeat --config config.yaml --output-dir ./output --cache-dir ./cache
```

### Options

- `--config FILE`: Path to YAML configuration file containing feed sources (required).
- `--output-dir DIR`: Directory where output Atom files will be written (required).
- `--cache-dir DIR`: Directory for cached Atom files (default: current directory).

## Configuration

Create a YAML file with a list of feed tasks:

```yaml
- sourceFeedUrl: "https://example.com/feed.atom"
  outputFilename: "unique-id-1"
  cacheSourceFeed: true
  repeatedEntryCount: 3
  minimumEntryAgeDays: 7

- sourceFeedUrl: "https://another-site.com/rss.xml"
  outputFilename: "unique-id-2"
  cacheSourceFeed: false
  repeatedEntryCount: 1
  minimumEntryAgeDays: 14
```

### Configuration Fields

- `sourceFeedUrl` (string, required): URL of the source feed to repeat from.
- `outputFilename` (string, required): Base filename for output Atom file (`.atom` extension added automatically).
- `cacheSourceFeed` (boolean, required): Whether to cache the source feed for fallback on network errors.
- `repeatedEntryCount` (integer, required): Number of entries to select for repetition per run.
- `minimumEntryAgeDays` (integer, required): Minimum age in days for entries to be eligible for selection.
- `minRunGapDays` (integer, optional, default: 1): Minimum gap in days between consecutive runs for this feed. Prevents the feed from being processed more frequently than specified.

## How It Works

1. **Fetch & Parse**: Downloads and parses the source feed, converting to Atom format if needed.
2. **Merge**: Combines source feed entries with existing output feed entries (deduplicating by link).
3. **Filter**: Removes entries younger than `minimumEntryAgeDays`.
4. **Select**: Randomly selects `repeatedEntryCount` entries using weighted sampling.
   - Weight increases exponentially with entry age.
   - This biases selection toward older entries, making them more likely to be repeated.
5. **Update**: Assigns new UUIDs and timestamps to the selected entries.
6. **Write**: Writes combined feed (new selections + existing output entries) to Atom file.
7. **Cache**: Optionally caches the fetched feed for use if future fetches fail.

Run frequency is limited to once per day per feed to avoid thrashing output feeds.

## Project Structure

- `src/Lib.hs`: Core library implementation
- `app/Main.hs`: Executable entry point
- `test/Main.hs`: Test suite
- `feed-repeat.cabal`: Build configuration
- `config.yaml`: Example configuration file
- `nix/`: Nix build files
- `nix/module.nix`: NixOS module

## Dependencies

### Core
- `feed`: RSS/Atom parsing and rendering
- `http-client`, `http-conduit`: HTTP requests with timeouts
- `time`: Timestamp handling
- `uuid`: Entry ID generation
- `random`: Random number generation
- `mtl`: Monad transformers for error handling

### Executable
- `aeson`: JSON encoding/decoding
- `yaml`: YAML configuration parsing
- `http-types`: HTTP types
- `optparse-applicative`: CLI argument parsing

### Testing
- `hspec`: Testing framework
- `QuickCheck`: Property-based testing

## License

MIT
