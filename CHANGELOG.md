# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - Unreleased

### Added
- Passthrough of new entries from source feed to output feed via `passthroughNewEntries` config option.
- SSRF protection for source feed URLs — DNS resolution and IP range validation before connecting.
- HTTP 429 (Too Many Requests) detection — treated like 304, falls back to cached feed.
- Source feed deduplication across tasks sharing the same URL.
- Canonicalization of output and cache directory paths.
- ETag and Last-Modified header persistence for conditional feed fetches.
- Configurable `User-Agent` header via `--user-agent` CLI option.
- Configurable service username in the NixOS module via the `userName` option.

### Changed
- Cache files are now per-task instead of per-source, enabling different `saveSourceFeedEntries` per task for the same source URL. Old cache files are migrated automatically to the new file names.

### Fixed
- HTTP retry for server errors — retry logic was previously broken because 5xx exceptions bypassed the retry loop. Server errors are now properly retried with exponential backoff.

### Removed
- Legacy cache file migration code.

## [1.0.0] - 2026-05-22

### Added
- Initial release of feed-repeat.
- Feed fetching from RSS, Atom, and RDF source feeds.
- Weighted random selection (A-Res) prioritizing older entries, with per-domain entry limits.
- Per-feed YAML configuration.
- Feed caching to disk.
- Output Atom feeds.
- Conditional fetches via `If-Modified-Since` header.
- HTTP response body size limit (10 MB), request timeout (30s), and exponential backoff retry.
- URL normalization and relative link resolution in feed entries.
- Tool CLI.
- NixOS module for declarative deployment with nginx reverse proxy.
- systemd service with sandboxing.
- Docker image running as non-root user (UID 1000).
- Example web server configs for nginx, Apache, and Caddy with HSTS and security headers.

[1.1.0]: https://github.com/abhin4v/feed-repeat/compare/1.0.0...main
[1.0.0]: https://github.com/abhin4v/feed-repeat/releases/tag/1.0.0
