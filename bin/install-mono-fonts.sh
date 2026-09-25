#!/usr/bin/env bash
set -e

FONT_DIR="${HOME}/.local/share/fonts"
FONTS=("googlesanscode" "inconsolata" "jetbrainsmono" "victormono" "cascadiamono")

echo "=== Installing Monospace Fonts (Google Sans Code, Inconsolata, JetBrains Mono, Victor Mono, Cascadia Mono) ==="

for font in "${FONTS[@]}"; do
    echo "--> Fetching font files for: $font..."
    TARGET_DIR="${FONT_DIR}/${font}"
    mkdir -p "${TARGET_DIR}"

    # Fetch file list from main folder and static folder if present via GitHub API
    urls=$(python3 - "$font" << 'EOF'
import sys, json, urllib.request

font_name = sys.argv[1]
urls = []

for subpath in ["", "/static"]:
    api_url = f"https://api.github.com/repos/google/fonts/contents/ofl/{font_name}{subpath}"
    req = urllib.request.Request(api_url, headers={"User-Agent": "Font-Installer-Script"})
    try:
        with urllib.request.urlopen(req) as resp:
            data = json.loads(resp.read().decode())
            if isinstance(data, list):
                for item in data:
                    if item.get("type") == "file" and item.get("name", "").lower().endswith((".ttf", ".otf")):
                        urls.append(item["download_url"])
    except Exception:
        pass

for u in urls:
    print(u)
EOF
    )

    if [ -z "$urls" ]; then
        echo "    Warning: No font files found for $font."
        continue
    fi

    while IFS= read -r url; do
        if [ -n "$url" ]; then
            raw_filename=$(basename "$url")
            # Unquote URL encoded characters (e.g. %5B -> [, %5D -> ])
            filename=$(python3 -c "import urllib.parse, sys; print(urllib.parse.unquote(sys.argv[1]))" "$raw_filename")
            echo "    Downloading ${filename}..."
            curl -sSL "$url" -o "${TARGET_DIR}/${filename}"
        fi
    done <<< "$urls"
done

echo "--> Updating system font cache (fc-cache)..."
fc-cache -f "${FONT_DIR}"

echo "=== Monospace fonts installation complete! ==="
