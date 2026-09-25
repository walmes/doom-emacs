#!/usr/bin/env bash
set -e

echo "=== Python Environment Setup for Doom Emacs ==="

# Find pip: Priority Anaconda > Miniconda > System pip
PIP_BIN=""

if [ -f "$HOME/anaconda3/bin/pip" ]; then
    PIP_BIN="$HOME/anaconda3/bin/pip"
elif [ -f "$HOME/miniconda3/bin/pip" ]; then
    PIP_BIN="$HOME/miniconda3/bin/pip"
elif command -v pip3 &> /dev/null; then
    PIP_BIN=$(command -v pip3)
elif command -v pip &> /dev/null; then
    PIP_BIN=$(command -v pip)
elif command -v python3 &> /dev/null && python3 -m pip --version &> /dev/null; then
    PIP_BIN="python3 -m pip"
fi

if [ -z "$PIP_BIN" ]; then
    echo "[Python] Warning: Could not find pip. Skipping pyright/ruff installation."
    exit 0
fi

echo "[Python] Installing/updating pyright and ruff using $PIP_BIN..."
$PIP_BIN install --upgrade pyright ruff

# Symlink binaries into ~/.local/bin if needed
LOCAL_BIN="$HOME/.local/bin"
mkdir -p "$LOCAL_BIN"

if [ -f "$HOME/anaconda3/bin/pyright" ]; then
    for tool in pyright pyright-langserver ruff; do
        if [ -f "$HOME/anaconda3/bin/$tool" ]; then
            echo "[Python] Linking $HOME/anaconda3/bin/$tool -> $LOCAL_BIN/$tool"
            ln -sf "$HOME/anaconda3/bin/$tool" "$LOCAL_BIN/$tool"
        fi
    done
elif [ -f "$HOME/miniconda3/bin/pyright" ]; then
    for tool in pyright pyright-langserver ruff; do
        if [ -f "$HOME/miniconda3/bin/$tool" ]; then
            echo "[Python] Linking $HOME/miniconda3/bin/$tool -> $LOCAL_BIN/$tool"
            ln -sf "$HOME/miniconda3/bin/$tool" "$LOCAL_BIN/$tool"
        fi
    done
fi

echo "=== Python Environment Setup Completed! ==="
