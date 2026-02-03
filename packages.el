;; -*- no-byte-compile: t; -*-
;; $DOOMDIR/packages.el

;;======================================================================
;; Doom Emacs Package Configuration
;;
;; Organized for readability and clarity.
;; To install a package, uncomment it or add it here, then run
;; 'doom sync'.
;;======================================================================

;;----------------------------------------------------------------------
;; 1. Visual Settings (Themes & Icons)
;;----------------------------------------------------------------------

;; Visual bookmarks in buffer.
;; https://github.com/joodland/bm
(package! bm)

;; Icons for Dired, Treemacs, and Mode-line.
;; https://github.com/domtronn/all-the-icons.el
(package! all-the-icons)

;; Nerd Font icons (alternative/complement to all-the-icons).
;; https://github.com/rainstormstudio/nerd-icons.el
(package! nerd-icons)

;; Nerd icons theme for Treemacs.
;; https://github.com/rainstormstudio/treemacs-nerd-icons
(package! treemacs-nerd-icons :pin "9876cb478145a0ec4e36f64ff6583f3de7126216")

;; Enhanced icons for Dired.
;; https://github.com/wyuenho/all-the-icons-dired
(package! all-the-icons-dired
  :recipe (:host github :repo "wyuenho/all-the-icons-dired"))

;; Perfect Margin for centering windows.
(package! perfect-margin)

;; Improved highlighting for programming modes.
;; https://codeberg.org/ideasman42/emacs-hl-prog-extra
(package! hl-prog-extra)

;; Better color handling for terminal/shell buffers.
;; https://github.com/atomontage/xterm-color
(package! xterm-color)

;; Visual Fill Column - centering text visually.
;; https://codeberg.org/joostkremers/visual-fill-column
(package! visual-fill-column)


;;----------------------------------------------------------------------
;; 2. Editor Enhancements
;;----------------------------------------------------------------------

;; Bookmark+ for advanced bookmark management.
;; https://github.com/emacsmirror/bookmark-plus
(package! bookmark+
  :recipe (:host github :repo "emacsmirror/bookmark-plus"))

;; Code folding based on indentation.
;; https://github.com/zenozeng/yafolding.el
(package! yafolding)

;; Sidebar for navigation (imenu).
;; https://github.com/bmag/imenu-list
(package! imenu-list)

;; Code screenshots.
;; https://github.com/tecosaur/screenshot
(package! screenshot
  :recipe (:host github :repo "tecosaur/screenshot"
           :build (:not compile))) ;; Workaround for build issue.


;;----------------------------------------------------------------------
;; 3. File Management
;;----------------------------------------------------------------------

;; Sidebar for file management (Dired integration).
;; https://github.com/jojojames/dired-sidebar
(package! dired-sidebar)


;;----------------------------------------------------------------------
;; 4. Programming Languages & Tools
;;----------------------------------------------------------------------

;;--- R / ESS ----------------------------------------------------------

;; View and manipulate data in ESS/R (tidyverse-like).
;; https://github.com/ShuguangSun/ess-view-data
(package! ess-view-data)

;; ESS Shell - Interact with shell from ESS.
;; Repo missing, so we download manually as requested.
(let ((essh-file (expand-file-name "essh.el" doom-user-dir)))
  (unless (file-exists-p essh-file)
    (url-copy-file
     "https://www.emacswiki.org/emacs/download/essh.el"
     essh-file))
  (package! essh
    :recipe (:local-repo "~/.doom.d"
             :files ("essh.el"))))

;; Electric Spacing for R - Smart operator spacing.
(package! electric-spacing-r
  :recipe (:host github
           :repo "walmes/electric-spacing"
           :files ("electric-spacing-r.el")))

;; Polymode stack for RMarkdown/Noweb.
(package! poly-noweb)
(package! poly-markdown)
(package! quarto-mode)

;; Orgalist - Org-like lists in other modes.
(package! orgalist)

;;--- Python -----------------------------------------------------------

;; ;; Elpy - Emacs Python Development Environment.
;; ;; https://elpy.readthedocs.io/en/latest/
;; (package! elpy)
;;
;; ;; Microsoft Python Language Server
;; ;; (disabled by default, using Pyright/Jedi usually).
;; (package! lsp-python-ms :disable t)

;;--- Stan -------------------------------------------------------------

;; Stan support for Bayesian modeling.
(package! stan-mode)
(package! company-stan)
(package! eldoc-stan)
(package! flycheck-stan)
(package! stan-snippets)

;;--- Presentation -----------------------------------------------------

;; Minimalist presentation tool for Org-mode.
;; https://github.com/rlister/org-present
(package! org-present
  :recipe (:host github :repo "rlister/org-present"))


;;----------------------------------------------------------------------
;; 5. AI & Code Assistance
;;----------------------------------------------------------------------

;; Copilot - GitHub Copilot client.
;; https://github.com/copilot-emacs/copilot.el
(package! copilot
  :recipe (:host github
           :repo "zerolfx/copilot.el"
           :files ("*.el" "dist")))

;; GPTel - Interaction with LLMs (ChatGPT, Gemini, etc.).
;; https://github.com/karthink/gptel
(package! gptel :recipe (:nonrecursive t))

;; Codeium - Free AI code completion.
;; https://github.com/Exafunction/codeium.el
;; (package! codeium
;;   :recipe (:host github
;;            :repo "Exafunction/codeium.el"))

;; Obsolete / Unused AI packages
;; (package! chatgpt :recipe (:host github :repo "emacs-openai/chatgpt"))
;; (package! c3po :recipe (:host github :repo "d1egoaz/c3po.el"))
;; (package! gpt)

;;----------------------------------------------------------------------
;; 6. Themes (Optional)
;;----------------------------------------------------------------------

;; Uncomment to install specific themes if not using Doom defaults.
;; (package! afternoon-theme)
;; (package! material-theme)
;; (package! solarized-theme)
