;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; Examples of using `el-get' to install packages.
;; https://tapoueh.org/blog/2010/08/el-get/
;; (package! el-get)

;; Visible bookmarks in buffer for GNU Emacs. Provides an easy way to
;; navigate in a buffer.
;; https://github.com/joodland/bm
(package! bm)

;; Bookmark+ significantly enhances the bookmark management experience
;; in Emacs by providing a comprehensive set of features for creating,
;; organizing, annotating, and navigating bookmarks.
;; https://github.com/emacsmirror/bookmark-plus
(package! bookmark+
  :recipe (:host github
           :repo "emacsmirror/bookmark-plus"))

;; Folding code blocks based on indentation.
;; https://github.com/zenozeng/yafolding.el
(package! yafolding)

;; Ease navigation in the buffer with list for language objects or
;; document sections.
;; https://github.com/bmag/imenu-list
(package! imenu-list)

;; This package provides an easy way to highlight words in programming
;; modes, where terms can be highlighted on code, comments or strings.
;; https://codeberg.org/ideasman42/emacs-hl-prog-extra
(package! hl-prog-extra)       ;; Complements `:ui hl-todo'.

;; ANSI, XTERM 256 and Truecolor support.
;; No probles with colors in the terminal.
;; https://github.com/atomontage/xterm-color
(package! xterm-color)

;; Provides a wide range of attractive icons that represent different
;; file types, directories, and special symbols, enriching the user
;; interface and making it more intuitive and visually appealing.
;; https://github.com/domtronn/all-the-icons.el
(package! all-the-icons)
;; TODO M-x all-the-icons-install-fonts

;; Nerd-icons.el is a library for easily using Nerd Font icons inside
;; Emacs, an alternative to all-the-icons. It works on both GUI and
;; terminal! You only need a Nerd Font installed on your system.
;; https://github.com/rainstormstudio/nerd-icons.el
(package! nerd-icons)
;; TODO M-x nerd-icons-install-fonts

;; `nerd-icons' theme for `treemacs'. It can be used inside GUI or
;; terminal.
;; https://github.com/rainstormstudio/treemacs-nerd-icons
(package! treemacs-nerd-icons :pin "9876cb478145a0ec4e36f64ff6583f3de7126216")

;; The Dired Sidebar package enriches the Emacs experience by providing
;; a user-friendly sidebar interface for file management tasks, with
;; navigation and Dired integration.
;; https://github.com/jojojames/dired-sidebar
(package! dired-sidebar)

;; `all-the-icons-dired' significantly improves the visual
;; representation of files and directories within Dired buffers, making
;; file navigation more intuitive and visually appealing.
;; https://github.com/wyuenho/all-the-icons-dired
(package! all-the-icons-dired
  :recipe (:host github
           :repo "wyuenho/all-the-icons-dired"))

;; With this package you can trivially convert a selected region of code
;; to a screenshot, with nothing but Emacs itself.
;; https://github.com/tecosaur/screenshot
(package! screenshot
  :recipe (:host github
           :repo "tecosaur/screenshot"
           ;; Avoid generate `.elc' that has a bug.
           ;; https://github.com/tecosaur/screenshot/issues/11
           :build (:not compile)))

;; This is meant to be an extremely minimalist presentation tool for
;; Emacs `org-mode'.
;; https://github.com/rlister/org-present
(package! org-present
  :recipe (:host github
           :repo "rlister/org-present"))

(package! perfect-margin)

;; Obsolete.
;; (package! smartparens)      ;; See `:config (default +smartparens)'.
;; (package! lsp-treemacs)     ;; Part of `:ui (treemacs +lsp)'.
;; (package! auctex)           ;; Part of `:lang latex'.
;; (package! yasnippet)        ;; Part of `:edit snippets'.

;; R ///////////////////////////////////////////////////////////////////

;; Polymode stack.
;; (package! polymode)         ;; Part of `:lang ess'.
;; (package! poly-R)           ;; Part of `:lang ess'.
;; (package! markdown-mode)    ;; Part of `:lang markdown'.
(package! poly-noweb)
(package! poly-markdown)
(package! quarto-mode)
(package! orgalist)

;; To do tidyverse-like view and manipulate data in ESS and R.
;; https://github.com/ShuguangSun/ess-view-data
(package! ess-view-data)

;; https://github.com/stan-dev/stan-mode?tab=readme-ov-file#configuration
(package! stan-mode)
(package! company-stan)
(package! eldoc-stan)
(package! flycheck-stan)
(package! stan-snippets)

;; Ease to read reports.
;; https://codeberg.org/joostkremers/visual-fill-column
(package! visual-fill-column)

;; Gets an error about unbalanced expressions.
;; (package! electric-spacing-r
;;   :recipe (:host github
;;            :repo "walmes/electric-spacing"
;;            ;; :branch "master"
;;            :files ("electric-spacing-r.el")
;;            ))

;; IA agents ///////////////////////////////////////////////////////////

;; Copilot.el is an Emacs plugin for GitHub Copilot.
;; https://github.com/copilot-emacs/copilot.el
;;
;; TODO Update NodeJS for Copilot.
;; $ curl -fsSL https://deb.nodesource.com/setup_current.x | sudo -E bash -
;; $ sudo apt-get install -y nodejs
(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))

;; GPTel is a simple Large Language Model chat client for Emacs, with
;; support for multiple models and backends. Backends includes
;; `ChatGPT', `Gemini' and many others.
;; https://github.com/karthink/gptel
;;
;; (package! gptel)

;; This Emacs Code extension allows you to use the official OpenAI API
;; to generate code or natural language responses from OpenAI's ChatGPT
;; to your questions, right within the editor.
;; https://github.com/emacs-openai/chatgpt
;;
;; (package! chatgpt
;;   :recipe (:host github :repo "emacs-openai/chatgpt"))

;; Codeium autocompletes your code with AI in all major IDEs. Is uses
;; Codeium AI and its FREE for users.
;; · https://codeium.com/blog/codeium-copilot-alternative-in-emacs
;; · https://codeium.com/emacs_tutorial
;; · https://github.com/Exafunction/codeium.el
;;
(package! codeium
  :recipe (:host github :repo "Exafunction/codeium.el"))

;; ChatGPT inside Emacs.
;; https://github.com/d1egoaz/c3po.el
;;
;; (package! c3po
;;   :recipe (:host github :repo "d1egoaz/c3po.el"))

;; `gpt.el' is a simple Emacs package that lets you interact with
;; instruction-following language models like ChatGPT and GPT-4 from
;; your editor.
;; https://github.com/stuhlmueller/gpt.el
;;
;; (package! gpt)

;; Python //////////////////////////////////////////////////////////////

;; Elpy is the Emacs Python Development Environment. It aims to provide
;; an easy to install, fully-featured environment for Python
;; development.
;; https://elpy.readthedocs.io/en/latest/
(package! elpy)

;; pyls.
;; pip install --user python-language-server[all]

;; pylsp.
;; pip install --user python-lsp-server

;; Microsoft language server.
;; (package! lsp-python-ms)            ;; Turn on.
(package! lsp-python-ms :disable t) ;; Turn off.
;; (package! lsp-pyright)

;; Color themes ////////////////////////////////////////////////////////

;; (package! afternoon-theme)
;; (package! apropospriate-theme)
;; (package! apropospriate-theme)
;; (package! fantom-theme)
;; (package! flatland-theme)
;; (package! flatui-theme)
;; (package! gotham-theme)
;; (package! leuven-theme)
;; (package! material-theme)
;; (package! molokai-theme)
;; (package! monokai-theme)
;; (package! solarized-theme)
;; (package! spacemacs-theme)
;; (package! vscode-dark-plus-theme)
