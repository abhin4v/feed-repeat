# feed-repeat

A Haskell tool that repeats entries from RSS/Atom feeds into new feeds. It fetches entries from source feeds, filters them by age, and selects a random subset for inclusion in output feeds using weighted sampling where older entries have higher priority.

## Features

- **Multi-feed support**: Process multiple source feeds with individual configurations.
- **Intelligent entry selection**: Uses exponential weighting to prioritize older entries.
- **Caching**: Optionally cache fetched feeds to handle source feed unavailability.
- **Filtering**: Filter entries by minimum age to avoid repeating recent content.
- **Format conversion**: Automatically converts RSS/RDF feeds to Atom format.

## Building

### Prerequisites

- GHC 9.10+ 
- Cabal 3.4+
- (Optional) Nix package and module support

### Build with Cabal

```bash
cabal build
```

### Build with Nix

Available scripts in Nix shell (defined in `scripts.nix`):

```bash
# Build the project
build

# Build a static binary
build-static

# Run the tool with example config
run
```

### Using as a NixOS Module

The project includes a NixOS module (`module.nix`) for easy integration into NixOS systems. Import it in your configuration:

```nix
{
  imports = [ ./feed-repeat/module.nix ];

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
- `outputFilename` (string, required): Base filename for output Atom file (`.atom` extension added. automatically)
- `cacheSourceFeed` (boolean, required): Whether to cache the source feed for fallback on network errors.
- `repeatedEntryCount` (integer, required): Number of entries to select for repetition per run.
- `minimumEntryAgeDays` (integer, required): Minimum age in days for entries to be eligible for selection.

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
- `module.nix`: NixOS module

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
