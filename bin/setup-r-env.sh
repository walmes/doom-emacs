#!/usr/bin/env bash
set -e

echo "=== R Environment Setup for Doom Emacs ==="

if ! command -v Rscript &> /dev/null; then
    echo "[R] Error: 'Rscript' executable not found on PATH."
    exit 1
fi

echo "[R] Checking and installing required R packages (languageserver, formatR, styler, stringi, lintr)..."
Rscript -e '
pkgs <- c("languageserver", "formatR", "styler", "stringi", "lintr")
installed <- installed.packages()[,"Package"]
missing_pkgs <- pkgs[!(pkgs %in% installed)]
if (length(missing_pkgs) > 0) {
  cat(sprintf("[R] Installing missing packages: %s\n", paste(missing_pkgs, collapse=", ")))
  install.packages(missing_pkgs, repos="https://cloud.r-project.org")
} else {
  cat("[R] All required R packages are already installed.\n")
}
'

echo "=== R Environment Setup Completed! ==="
