#!/usr/bin/env nix-shell
#! nix-shell -i bash -p pandoc
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
mkdir -p docs
touch docs/.nojekyll

# Generate index.html from README
pandoc --standalone  --css light.css --metadata title="feed-repeat" README.md -o docs/index.html

# Generate CHANGELOG.html from CHANGELOG
pandoc --standalone  --css light.css --metadata title="Changelog — feed-repeat" CHANGELOG.md -o docs/CHANGELOG.html

# Generate hosting-on-github-pages.html from docs/hosting-on-github-pages.md
pandoc --standalone  --css light.css --metadata title="Hosting on GitHub Pages — feed-repeat" docs/hosting-on-github-pages.md -o docs/hosting-on-github-pages.html

# Generate docs/nix-module-options.md from the Nix module, then render to HTML
gen-nix-module-docs
pandoc --standalone  --css light.css --metadata title="NixOS Module Options — feed-repeat" docs/nix-module-options.md -o docs/nix-module-options.html

# Fix CHANGELOG.md link in index.html to point to CHANGELOG.html
sed -i 's#href="CHANGELOG\.md"#href="CHANGELOG.html"#g' docs/index.html

# Remove the title-block-header (pandoc duplicates the title from --metadata)
for f in docs/index.html docs/CHANGELOG.html docs/hosting-on-github-pages.html docs/nix-module-options.html; do
  sed -i '/^<header id="title-block-header">$/,/^<\/header>$/d' "$f"
done

# Add navigation bar to all pages (after <body>)
for f in docs/index.html docs/CHANGELOG.html docs/hosting-on-github-pages.html docs/nix-module-options.html; do
  sed -i "s#<body>#<body>\n<nav><a href=\"index.html\">Home</a><a href=\"nix-module-options.html\">NixOS Options</a><a href=\"CHANGELOG.html\">Changelog</a><a href=\"https://github.com/abhin4v/feed-repeat\">Source</a></nav>\n#" "$f"
  sed -i "s#</body>#<footer>Made with <a href=\"https://www.haskell.org/\">Haskell</a> by <a href=\"https://abhinavsarkar.net/\">Abhinav Sarkar</a></footer>\n</body>#" "$f"
done

echo "Site built in docs/"
