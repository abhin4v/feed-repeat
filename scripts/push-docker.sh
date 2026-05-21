#!/usr/bin/env bash
set -euo pipefail

# Usage: ./push-docker.sh <version> <tarball-directory> [--latest]
# Example: ./push-docker.sh 1.0.0 /tmp/fr --latest
#
# Requires:
# - podman
# - Files in <tarball-directory>:
#   - docker-image-feed-repeat-amd64.tar.gz
#   - docker-image-feed-repeat-aarch64.tar.gz
#
# Environment:
# - GHCR_TOKEN: GitHub token with write:packages scope (or use podman login interactively)
#
# Options:
#   --latest    Also tag and push the multi-arch manifest as :latest.
#               Off by default so a half-baked build can't accidentally
#               become the rolling tag.

VERSION="${1:?Usage: $0 <version> <tarball-directory> [--latest]}"
TAR_DIR="${2:?Usage: $0 <version> <tarball-directory> [--latest]}"
PUSH_LATEST=false
if [ "${3:-}" = "--latest" ]; then
  PUSH_LATEST=true
fi
GHCR_USER="${GHCR_USER:?Set GHCR_USER (e.g., your GitHub username)}"
IMAGE="ghcr.io/${GHCR_USER}/feed-repeat"

# Load images
echo "Loading amd64 image..."
podman load < "${TAR_DIR}/docker-image-feed-repeat-amd64.tar.gz"

echo "Loading aarch64 image..."
podman load < "${TAR_DIR}/docker-image-feed-repeat-aarch64.tar.gz"

# Tag images
echo "Tagging images..."
podman tag localhost/feed-repeat:latest "${IMAGE}:${VERSION}-amd64"
podman tag localhost/feed-repeat:latest "${IMAGE}:${VERSION}-arm64"

# Login to GHCR
if [ -n "${GHCR_TOKEN:-}" ]; then
  echo "Logging in to GHCR..."
  echo "${GHCR_TOKEN}" | podman login ghcr.io -u "${GHCR_USER}" --password-stdin
else
  echo "No GHCR_TOKEN set. You'll need to log in manually."
  podman login ghcr.io -u "${GHCR_USER}"
fi

# Push images, capturing digests so the manifest references immutable
# content rather than mutable tags (which could be swapped between push
# and manifest creation).
DIGEST_DIR="$(mktemp -d)"
trap 'rm -rf "${DIGEST_DIR}"' EXIT

echo "Pushing amd64 image..."
podman push --digestfile "${DIGEST_DIR}/amd64.digest" "${IMAGE}:${VERSION}-amd64"
AMD64_DIGEST="$(cat "${DIGEST_DIR}/amd64.digest")"
echo "  amd64 digest: ${AMD64_DIGEST}"

echo "Pushing arm64 image..."
podman push --digestfile "${DIGEST_DIR}/arm64.digest" "${IMAGE}:${VERSION}-arm64"
ARM64_DIGEST="$(cat "${DIGEST_DIR}/arm64.digest")"
echo "  arm64 digest: ${ARM64_DIGEST}"

# Create and push multi-arch manifest by digest
echo "Creating multi-arch manifest for ${VERSION}..."
podman manifest create "${IMAGE}:${VERSION}"
podman manifest add "${IMAGE}:${VERSION}" "docker://${IMAGE}@${AMD64_DIGEST}"
podman manifest add "${IMAGE}:${VERSION}" "docker://${IMAGE}@${ARM64_DIGEST}"

echo "Pushing manifest ${IMAGE}:${VERSION}..."
podman manifest push "${IMAGE}:${VERSION}"

if [ "${PUSH_LATEST}" = "true" ]; then
  echo "Creating latest manifest..."
  podman manifest create "${IMAGE}:latest"
  podman manifest add "${IMAGE}:latest" "docker://${IMAGE}@${AMD64_DIGEST}"
  podman manifest add "${IMAGE}:latest" "docker://${IMAGE}@${ARM64_DIGEST}"

  echo "Pushing latest manifest..."
  podman manifest push "${IMAGE}:latest"
else
  echo "Skipping :latest tag (pass --latest to enable)."
fi

echo "Done!"
echo "Images pushed:"
echo "  ${IMAGE}:${VERSION}"
if [ "${PUSH_LATEST}" = "true" ]; then
  echo "  ${IMAGE}:latest"
fi
