#-----------------------------------------------------------------------
# Install Doom-Emacs and add personal configuration.

# Keep a copy of `.emacs.d/`.
rm -rf ~/OLD.emacs.d/
mv -v ~/.emacs.d ~/OLD.emacs.d/

# Install Doom-Emacs.
# git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install

# Brings my persional configuration.
rm -rf ~/.doom.d/
git clone git@github.com:walmes/doom-emacs.git ~/.doom.d/

# Upgrade Doom-emacs.
~/.emacs.d/bin/doom upgrade

#-----------------------------------------------------------------------
# Remove Emacs.

# Check current version and components to be uninstalled.
emacs --version
dpkg --get-selections | grep emacs | cut -f1

# Uninstall them.
sudo apt purge `dpkg --get-selections | grep emacs | cut -f1`

# Install a specific version.
sudo apt-get install emacs28

# Adds PPA for the latest release.
sudo add-apt-repository ppa:ubuntuhandbook1/emacs
sudo apt update

# Installs.
# sudo apt cache search emacs
sudo apt install emacs emacs-common

# Check the installed version.
emacs --version

# TODO: Install or update doom emacs.

#-----------------------------------------------------------------------
# Rscript -e 'install.packages(c("languageserver", "formatR"))'

#-----------------------------------------------------------------------
# Install some packages manually.

# (progn
#   (require 'package)
#   (package-initialize)
#   (when (not (package-installed-p 'electric-spacing-r))
#     (url-copy-file
#      "https://raw.githubusercontent.com/walmes/electric-spacing/master/electric-spacing-r.el"
#      "~/.doom.d/electric-spacing-r.el")
#     (byte-compile-file "~/.doom.d/electric-spacing-r.el")))

# (progn
#   (require 'package)
#   (package-initialize)
#   (when (not (package-installed-p 'essh))
#     (url-copy-file
#      "https://www.emacswiki.org/emacs/download/essh.el"
#      "~/.doom.d/essh.el")
#     (byte-compile-file "~/.doom.d/essh.el")))

# Requirements for `python` module.
# https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/python
#   cd anaconda
#   source activate
#   pip install pytest nose black pyflakes isort
#   pip install python-language-server[all]
#   pip install pyright
#   conda deactivate
# On Emacs, after refreshing it:
#   M-x lsp-install-server RET mspyls
# https://github.com/microsoft/pyright

# Requirements for `(cc +lsp)` module.
# sudo apt-get install clangd-10

# Requirements for `ess` module.
LIB="/usr/lib/R/site-library"
REPOS="http://cran-r.c3sl.ufpr.br/"
CMD="lib <- \"$LIB\"; repos <- \"$REPOS\"; install.packages(c(\"formatR\", \"lintr\", \"languageserver\"), dependencies = TRUE, lib = lib, repos = repos)"
sudo Rscript -e "$CMD"
