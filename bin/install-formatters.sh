#!/usr/bin/env bash
set -e

echo "=== Doom Emacs Formatter Setup ==="

# 1. Install Panache if missing
if ! command -v panache &> /dev/null; then
    echo "[Panache] Installing Panache CLI..."
    curl --proto '=https' --tlsv1.2 -sSf https://panache.bz/install | sh
else
    echo "[Panache] Panache CLI is already installed ($(panache --version 2>/dev/null || echo 'installed'))."
fi

# 2. Install Air if missing
if ! command -v air &> /dev/null; then
    echo "[Air] Installing Air R Formatter CLI..."
    curl -LsSf https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | sh
else
    echo "[Air] Air CLI is already installed ($(air --version 2>/dev/null || echo 'installed'))."
fi

# 3. Ensure configuration directories exist
mkdir -p "$HOME/.config/panache"
mkdir -p "$HOME/.config/air"

# 4. Link configuration files from ~/.doom.d/configs/
CONFIG_DIR="$HOME/.doom.d/configs"

if [ -f "$CONFIG_DIR/panache.toml" ]; then
    echo "[Panache] Linking $CONFIG_DIR/panache.toml -> $HOME/.config/panache/config.toml"
    ln -sf "$CONFIG_DIR/panache.toml" "$HOME/.config/panache/config.toml"
fi

if [ -f "$CONFIG_DIR/air.toml" ]; then
    echo "[Air] Linking $CONFIG_DIR/air.toml -> $HOME/.config/air/air.toml"
    ln -sf "$CONFIG_DIR/air.toml" "$HOME/.config/air/air.toml"
fi

echo "=== Setup completed successfully! ==="
