#!/usr/bin/env bash
set -e

echo "=== Installing Emacs Icon Fonts (all-the-icons & nerd-icons) ==="

emacs --batch --eval '
(progn
  (require "all-the-icons" nil t)
  (if (fboundp "all-the-icons-install-fonts")
      (all-the-icons-install-fonts t)
    (message "[all-the-icons] Function all-the-icons-install-fonts not available.")))
'

emacs --batch --eval '
(progn
  (require "nerd-icons" nil t)
  (if (fboundp "nerd-icons-install-fonts")
      (nerd-icons-install-fonts t)
    (message "[nerd-icons] Function nerd-icons-install-fonts not available.")))
'

echo "=== Icon fonts installation finished ==="
