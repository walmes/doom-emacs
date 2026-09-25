;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;======================================================================
;; Doom Emacs Configuration by Walmes Zeviani
;;
;; This file is hosted at https://github.com/walmes/doom-emacs.
;;
;; Organized for readability, performance, and maintainability.
;; content extracted and refactored from the original `config.el`.
;;======================================================================

;;----------------------------------------------------------------------
;; 1. User Identity
;;----------------------------------------------------------------------

(setq user-full-name "Walmes Zeviani"
      user-mail-address "walmeszeviani")

;;----------------------------------------------------------------------
;; 2. Visual Settings (Fonts & Themes)
;;----------------------------------------------------------------------

;; Fonts
;; doom-font: Primary font used for code.
;; doom-variable-pitch-font: Font for non-monospaced text (like Org headings).
;; doom-big-font: Used when `doom-big-font-mode` is enabled (for presentations).
;; (setq doom-font (font-spec :family "Fira Mono" :weight 'light)
;;       doom-big-font (font-spec :size 21))

;; Theme
;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function.
(setq doom-theme 'doom-one)
;; Alternative themes commented out:
;; (setq doom-theme 'doom-bluloco-dark)
;; (setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-city-lights)
;; (setq doom-theme 'doom-tokyo-night)
;; (setq doom-theme 'doom-ayu-dark)

;; Line Numbers
;; This determines the style of line numbers in effect. If set to `nil',
;; line numbers are disabled. For relative line numbers, set this to
;; `relative'.
(setq display-line-numbers-type nil)

;; Transparency (optional)
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

;; Maximize frame on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Margin settings
(setq scroll-margin 5)
(dolist (mode '(messages-buffer-mode-hook
                comint-mode-hook
                term-mode-hook
                erc-mode-hook
                inferior-ess-mode-hook
                eshell-mode-hook
                inferior-python-mode-hook))
  (add-hook mode (lambda () (set (make-local-variable 'scroll-margin) 0))))

;;----------------------------------------------------------------------
;; 3. Core Editor Configuration
;;----------------------------------------------------------------------

(setq org-directory "~/org/")

;; Basic Modes
(global-hl-line-mode 1)         ; Highlight the cursor line.
(visual-line-mode 1)            ; Screen lines, not logical lines.
(show-paren-mode 1)             ; Highlight matching pairs.
(delete-selection-mode 1)       ; Replace selection when typing.
(recentf-mode 1)                ; Keep track of recently opened files.
(global-auto-revert-mode 1)     ; Refresh buffer if file changes.

;; General Settings
(setq column-number-mode t      ; Show cursor column position.
      auto-save-default nil     ; Turn off #autosave# files.
      make-backup-files nil     ; Turn off backup~ files.
      comment-empty-lines t     ; Apply comments to empty lines.
      select-enable-clipboard t ; Integrate with system clipboard.
      tab-always-indent t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 72)

;; Global Whitespace Management
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(face trailing tabs empty))
(global-whitespace-mode t)

(custom-set-faces!
  ;; '(whitespace-line
  ;;   :background nil
  ;;   :foreground "#f0c674"
  ;;   :weight normal)
  '(fill-column-indicator
    :foreground "#3f444a"
    :background nil
    :weight light))

;; Whitespace Configuration for Programming & Config Modes
(setq-hook! '(prog-mode-hook conf-mode-hook)
  display-fill-column-indicator-column fill-column
  whitespace-line-column fill-column
  ;; whitespace-style '(face lines-tail trailing tabs empty)
  whitespace-style '(face trailing tabs empty))

;; Whitespace Configuration for Data & Config Files
(setq-hook! '(json-mode-hook
              csv-mode-hook
              yaml-mode-hook
              toml-mode-hook
              dcf-mode-hook
              bibtex-mode-hook
              latex-mode-hook
              TeX-mode-hook
              plain-TeX-mode-hook)
    whitespace-style '(face trailing tabs empty))

;; `C-c t c' to toggle fill-column-indicator mode.
(add-hook! '(prog-mode-hook conf-mode-hook)
           #'display-fill-column-indicator-mode
           #'rainbow-mode)

;----------------------------------------------------------------------
;; 4. Global Keybindings
;;----------------------------------------------------------------------
;; Using Doom's `map!` macro for cleaner key definitions.
;; See `M-x doom/help-modules` -> `config/default` or
;; `C-h f map!` for docs.

(map!
 ;; Undo/Files
 "C-z"       #'undo
 "C-/"       #'company-files

 ;; Functions previously bound to global keys
 "C-~"           #'fixup-whitespace
 "M-<delete>"    #'fixup-whitespace
 "M-~"           #'delete-indentation
 "S-<backspace>" #'delete-indentation

 ;; Frame toggles
 "<S-f11>" #'toggle-menu-bar-mode-from-frame
 "<S-f12>" #'toggle-tool-bar-mode-from-frame
 "<f5>"    #'revert-buffer

 ;; Bookmark+
 ;; Note: Ensure `bookmark+` is installed.
 "<C-f3>"  #'bmkp-toggle-autonamed-bookmark-set/delete
 "<f3>"    #'bmkp-next-bookmark-this-file/buffer-repeat
 "<f4>"    #'bmkp-previous-bookmark-this-file/buffer-repeat
 "<S-f3>"  #'bmkp-toggle-temporary-bookmark

 ;; Visible Bookmarks (bm)
 "<C-f2>"  #'bm-toggle
 "<f2>"    #'bm-next
 "<S-f2>"  #'bm-previous

 ;; Imenu-list
 "<f12>"   #'imenu-list-smart-toggle

 ;; YaFolding
 "C-{"     #'yafolding-hide-parent-element
 "C-}"     #'yafolding-toggle-element)


;;----------------------------------------------------------------------
;; 5. Modules & Packages
;;----------------------------------------------------------------------

;;--- Loading External Files -------------------------------------------
;; Recommended: Add `funcs.el` to the same directory as config.el
(add-load-path! ".")
(require 'funcs nil t) ; Load if exists, don't error if not

;;--- Company (Completion) ---------------------------------------------
(use-package! company
  :bind
  ("C-*" . company-complete))

(after! company
  (map! :map company-active-map
        "TAB"      #'company-complete-selection
        "<tab>"    #'company-complete-selection
        "RET"      #'company-complete-selection
        "<return>" #'company-complete-selection))

;;--- Magit (Git) ------------------------------------------------------
(use-package! magit
  :bind
  ("C-c g" . magit-status))

;;--- Bookmarks (Bookmark+) --------------------------------------------
;; Use `package!` in packages.el to install bookmark+
(use-package! bookmark+
  :init
  (when (file-exists-p "~/Dropbox/bookmarks")
    (setq bookmark-default-file "~/Dropbox/bookmarks"
          bookmark-save-flag 1)))

;;--- Visible Bookmarks (bm) -------------------------------------------
(use-package! bm
  :config
  (setq bm-marker 'bm-marker-left
        bm-highlight-style 'bm-highlight-only-fringe))

;;--- Folding (YaFolding) ----------------------------------------------
(use-package! yafolding)

;;--- Snippets (Yasnippet) ---------------------------------------------
(use-package! yasnippet
  :config
  (yas-global-mode 1))

;;--- Web Mode ---------------------------------------------------------
(use-package! web-mode
  :config
  (setq web-mode-markup-indent-offset 4))

;;--- Treemacs ---------------------------------------------------------
(use-package! treemacs
  :config
  (setq treemacs-is-never-other-window nil))

(use-package! treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package! nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;--- Dired Sidebar ----------------------------------------------------
(use-package! dired-sidebar
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__"
        dired-sidebar-theme 'ascii
        dired-sidebar-use-term-integration t
        dired-sidebar-use-custom-font nil))

;;--- Imenu List -------------------------------------------------------
(use-package! imenu-list
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-size 0.2
        imenu-list-position 'left))

;;--- Screenshot -------------------------------------------------------
(use-package! screenshot
  :config
  (setq screenshot-schemes '(("current-directory"
                              :dir default-directory))
        screenshot-default-scheme "current-directory"))

;; Define `screenshot-font-family' in the `custom.el' to avoid issues.
(after! screenshot
  (setq screenshot-font-size 12
        screenshot-line-numbers-p nil
        screenshot-relative-line-numbers-p nil
        screenshot-remove-indent-p t
        screenshot-shadow-offset-horizontal 0
        screenshot-shadow-offset-vertical 0)
  ;; Override the original package function to save the image with
  ;; timestamp.
  (screenshot--def-action save
    "Save the current selection with filename and timestamp to avoid
     always overwriting the same file."
    (let* ((base (file-name-sans-extension
                  (or (buffer-file-name)
                      (expand-file-name "screenshot" default-directory))))
           ;; Generate date and time suffix.
           (timestamp (format-time-string "-%Y-%m-%d_%H-%M-%S"))
           (final-path (concat base timestamp ".png")))
      (rename-file screenshot--tmp-file final-path t)
      (message "Screenshot saved as: %s" final-path))))

;;--- Visual Fill Column -----------------------------------------------

(setq visual-fill-column-center-text t)

(defun wz-visual-fill-column-width-by-extension ()
  "Set `visual-fill-column-width' for specific text/document extensions."
  (when (bound-and-true-p visual-fill-column-mode)
    (let ((ext (when buffer-file-name
                 (downcase (file-name-extension buffer-file-name)))))
      (setq-local visual-fill-column-width
                  (if (member ext '("md" "rmd" "qmd" "tex" "rnw"))
                      96
                    nil)))))

(use-package! visual-fill-column
  :config
  (setq visual-fill-column-center-text t)
  (add-hook 'visual-fill-column-mode-hook
            #'wz-visual-fill-column-width-by-extension)
  (add-hook! '(markdown-mode-hook
              quarto-mode-hook
              org-mode-hook)
             #'visual-fill-column-mode))

;;--- Org Mode & Presentation ------------------------------------------

(use-package! org-present
  :config
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-display-inline-images)
              (setq header-line-format " ")
              (visual-fill-column-mode t)
              (visual-line-mode t)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-remove-inline-images)
              (setq header-line-format nil)
              (visual-fill-column-mode nil)
              (visual-line-mode nil))))

;;--- Perfect Margin ---------------------------------------------------
(use-package! perfect-margin
  :config
  (after! doom-modeline
    (setq mode-line-right-align-edge 'right-fringe))
  (after! minimap
    (setq minimap-width-fraction 0.08)))

;;--- Xterm Color Support (R, Python, Shell) -------------------------
(use-package! xterm-color
    :config
    (setq xterm-color-use-bold t))

(after! comint
    ;; Remove the native filter to avoid processing conflicts.
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output
                  comint-output-filter-functions))
    ;; Apply the xterm-color filter to all comint buffers.
    (add-hook 'comint-mode-hook
              (lambda ()
                  ;; Add the filter locally to the buffer.
                  (add-hook 'comint-preoutput-filter-functions
                            #'xterm-color-filter
                            nil
                            t)))
    ;; Ensure processes know that Emacs accepts 256 colors.
    (setenv "TERM" "xterm-256color"))

;;--- Markdown ---------------------------------------------------------
(use-package! markdown-mode
  :config
  (require 'orgalist)
  (orgalist-mode t)
  (add-hook 'markdown-mode-hook #'imenu-add-menubar-index)
  ;; Key Mapping.
  (map! :map markdown-mode-map
        "C-c *" #'orgalist-cycle-bullet
        "<f10>" #'imenu-list-smart-toggle))

;;--- Markdown / Quarto / Rmd Formatter CLI (Panache / Remark) ---------
;; Panache (Default): https://github.com/jolars/panache (Rust, Pandoc/Quarto native)
;;   Install Panache with: curl --proto '=https' --tlsv1.2 -sSf https://panache.bz/install | sh
;;   Create global config file in ~/.config/panache/config.toml
;; Remark: https://remark.js.org (Node.js Unified.js)
;; Functions implemented in `funcs.el`.
;; Interactive selection: M-x markdown-select-formatter
;; Interactive toggle: M-x markdown-format-on-save-toggle
;; Manual format: M-x panache-format-buffer, M-x remark-format-buffer, M-x markdown-format-buffer.
;; Formatter choice configured in `wz-markdown-formatter` ('panache or 'remark).

(defun wz-markdown-setup-format-on-save ()
  "Setup Markdown/Quarto/Rmd format-on-save buffer-locally."
  (let ((exec-name (symbol-name (or (bound-and-true-p wz-markdown-formatter) 'panache))))
    (when (and (executable-find exec-name)
               (or (derived-mode-p 'markdown-mode 'gfm-mode)
                   (bound-and-true-p poly-quarto-mode)
                   (bound-and-true-p poly-markdown+r-mode)
                   (memq major-mode '(poly-quarto-mode poly-markdown+r-mode poly-markdown-mode quarto-mode))
                   (when-let* ((file (buffer-file-name)))
                     (member (downcase (file-name-extension file)) '("md" "markdown" "rmd" "qmd")))))
      (make-local-variable 'wz-markdown-format-on-save)
      (add-hook 'before-save-hook #'wz-markdown-format-on-save-maybe nil t))))

(defalias 'wz-remark-setup-format-on-save #'wz-markdown-setup-format-on-save)

;; Hook into Markdown, Quarto, and Polymode modes, as well as file open
(add-hook! '(markdown-mode-hook
             gfm-mode-hook
             quarto-mode-hook
             poly-quarto-mode-hook
             poly-markdown+r-mode-hook
             poly-markdown-mode-hook)
           #'wz-markdown-setup-format-on-save)
(add-hook 'find-file-hook #'wz-markdown-setup-format-on-save)

;; Compatibility with Doom's :editor (format +onsave) / Apheleia if ever enabled
(when (fboundp 'set-formatter!)
  (set-formatter! 'panache-markdown
    '("panache"
      "format"
      "--stdin-filename" (or (buffer-file-name) "temp.qmd")
      "-")
    :modes '(markdown-mode
             gfm-mode
             quarto-mode
             poly-quarto-mode
             poly-markdown+r-mode
             poly-markdown-mode))

  (set-formatter! 'remark-markdown
    '("remark"
      "-S"
      (when (file-exists-p (expand-file-name "~/.remarkrc.json"))
        (list "--rc-path" (expand-file-name "~/.remarkrc.json"))))
    :modes '(markdown-mode
             gfm-mode
             quarto-mode
             poly-quarto-mode
             poly-markdown+r-mode
             poly-markdown-mode)))

;;--- ESSH (Emacs Speaks Statistics Shell) -----------------------------
(use-package! essh
  :config
  ;; 1. "Smart" function for Ctrl + Enter
  (defun wz-sh-send-line-or-region-and-step ()
    "If there is an active region, send it to the shell. Otherwise, send the line and step."
    (interactive)
    (if (use-region-p)
        (progn
          (pipe-region-to-shell)
          (deactivate-mark))
      (pipe-line-to-shell-and-step)))
  ;; 2. Key Mapping
  (after! sh-script
    (map! :map sh-mode-map
          "C-<return>"  #'wz-sh-send-line-or-region-and-step
          "C-c C-r"     #'pipe-region-to-shell
          "C-c C-b"     #'pipe-buffer-to-shell
          "C-c C-j"     #'pipe-line-to-shell
          "C-c C-n"     #'pipe-line-to-shell-and-step
          "C-c C-f"     #'pipe-function-to-shell
          "C-c C-d"     #'shell-cd-current-directory))
  ;; 3. Imenu Configuration (Code Sections)
  (add-hook! 'sh-mode-hook
    (setq imenu-generic-expression
          (append '(("Blocks" "^#-\\{0,4\\} \\(.\\{1,15\\}\\)[^-]* ---+$" 1))
                  (nthcdr 1 (car sh-imenu-generic-expression))))))

;;--- Hi-Lock ----------------------------------------------------------
(use-package! hi-lock)

;;----------------------------------------------------------------------
;; 6. Programming Languages
;;----------------------------------------------------------------------

;;--- LSP UI -----------------------------------------------------------
(use-package! lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-imenu-buffer-position 'left)
  (lsp-headerline-breadcrumb-enable t)
  :config
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (map! :map lsp-ui-mode-map
        "C-c u" #'lsp-ui-doc-show
        "M-i"   #'lsp-ui-doc-focus-frame)
  (map! :map lsp-mode-map
        "M-n"   #'forward-paragraph
        "M-p"   #'backward-paragraph)
  ;; Hide mode-line of the lsp-ui-imenu buffer
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

;;--- LSP Treemacs -----------------------------------------------------
(use-package! lsp-treemacs
  :bind
  ("C-<f8>" . lsp-treemacs-symbols-toggle)
  ("<f8>"   . lsp-ui-imenu-toggle))

;;--- R (ESS) ----------------------------------------------------------
(use-package! ess
  :init
  (progn
    (setq-default ess-dialect "R")
    (setq-default inferior-R-args "--no-restore-history --no-save ")
    ;; (setq inferior-ess-r-program "/home/walmes/anaconda3/bin/R")
    (setq ess-indent-with-fancy-comments nil
          comint-scroll-to-bottom-on-input t
          comint-scroll-to-bottom-on-output t
          comint-move-point-for-output t
          ess-indent-offset 4)
    (setq ess-use-flymake nil))
  :bind
  (("C-S-<f5>" . ess-eval-chunk)
   ("C-S-<f6>" . ess-eval-chunk-and-step)
   ("C-S-<f7>" . ess-noweb-next-code-chunk)
   ("C-S-<f8>" . ess-noweb-previous-code-chunk)
   ("C-S-<f9>" . ess-noweb-goto-chunk)
   ("C-|"      . " |>")) ; Native pipe
  :config
  ;; Font lock settings
  (setq ess-R-font-lock-keywords
        '((ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (setq inferior-R-font-lock-keywords
        '((ess-S-fl-keyword:prompt . t)
          (ess-R-fl-keyword:messages . t)
          (ess-R-fl-keyword:modifiers . t)
          (ess-R-fl-keyword:fun-defs . t)
          (ess-R-fl-keyword:keywords . t)
          (ess-R-fl-keyword:assign-ops . t)
          (ess-R-fl-keyword:constants . t)
          (ess-fl-keyword:matrix-labels . t)
          (ess-fl-keyword:fun-calls . t)
          (ess-fl-keyword:numbers . t)
          (ess-fl-keyword:operators . t)
          (ess-fl-keyword:delimiters . t)
          (ess-fl-keyword:= . t)
          (ess-R-fl-keyword:F&T . t)))
  (add-hook
   'ess-mode-hook
   '(lambda ()
      ;; OBSOLETE: Do not load ess-site to avoid loading deprecated package ess-jags-d
      (require 'ess-view-data)
      (setq ess-smart-operators t)
      (setq-local comment-add 0) ;; Single # as default.
      (ess-set-style 'RStudio)
      ;; LSP & Company configuration
      (setq lsp-enable-symbol-highlighting nil
            lsp-signature-auto-activate nil
            lsp-ui-doc-enable nil
            lsp-diagnostics-provider :none
            lsp-restart 'ignore)
      ;; Keybindings
      (define-key ess-mode-map [?\M--] 'ess-cycle-assign)
      (define-key ess-mode-map [S-f5] 'company-R-args)
      (define-key ess-mode-map [C-f5] 'company-R-objects)
      )
   )
  (add-hook
   'inferior-ess-mode-hook
   '(lambda ()
      (setq-local comint-use-prompt-regexp nil)
      (setq-local inhibit-field-text-motion nil)))
  ;; Prevent accidental buffer evaluation
  (defadvice ess-eval-buffer (before really-eval-buffer compile activate)
    (if (yes-or-no-p
         (format "Are you sure you want to evaluate the %s buffer?"
                 buffer-file-name))
        (message "ess-eval-buffer started.")
      (error "ess-eval-buffer canceled!"))))

;; Turn off flycheck in LSP for R (ESS) buffers.
(add-hook! 'lsp-mode-hook
    (defun wz-disable-flycheck-in-lsp-selected-modes ()
        "Disable flycheck in LSP only for R (ESS) and Python."
        (when (derived-mode-p 'ess-mode 'python-mode)
            (flycheck-mode -1))))

;;--- Projectile & LSP Project Roots -----------------------------------
(after! projectile
  (add-to-list 'projectile-project-root-files ".Rproj")
  (add-to-list 'projectile-project-root-files ".here"))

(after! lsp-mode
  ;; Automatically restrict LSP workspace root:
  ;; 1. If inside a Git/Projectile repository, use project root.
  ;; 2. If NOT a Git project, use the file's own directory (default-directory),
  ;;    never encompassing broad parent folders such as ~/Projects.
  (setq lsp-auto-guess-root t)

  ;; Disable excessive file watchers to prevent sluggishness
  (setq lsp-enable-file-watchers nil)

  ;; Ensure standard lsp-r remains active for symbols (lsp-treemacs-symbols-toggle),
  ;; navigation, and documentation, delegating formatting on save to Air CLI
  (setq lsp-disabled-clients (delq 'lsp-r lsp-disabled-clients))
  (setq lsp-format-buffer-on-save nil))

;;--- Air (R Formatter CLI) --------------------------------------------
;; Air: https://github.com/posit-dev/air
;; Functions implemented in `funcs.el' (Section 4. R / ESS).
;; Interactive toggle: M-x air-lsp-format-on-save-toggle (or air-format-on-save-toggle).
;; Initial default value configured in `custom.el' (`wz-air-format-on-save').
(when (executable-find "air")
  (add-hook! 'ess-r-mode-hook
    (make-local-variable 'wz-air-format-on-save)
    (add-hook 'before-save-hook #'wz-air-format-on-save-maybe nil t)))


;;--- Electric Spacing (R) ---------------------------------------------
(use-package! electric-spacing-r
  :config
  (add-hook 'ess-mode-hook #'electric-spacing-mode)
  (add-hook 'python-mode-hook #'electric-spacing-mode))

;;--- Polymode/Quarto --------------------------------------------------

;; Fix for the weirdness of polymode and lsp. See for more info:
;; https://github.com/polymode/poly-R/issues/34
(setq polymode-lsp-integration nil)

;;--- Quarto -----------------------------------------------------------
(use-package! quarto-mode)

;;--- Python -----------------------------------------------------------
;; Requirements & setup for a VS Code / Antigravity equivalent experience:
;;
;; 1. Doom Module (`init.el'):
;;    Enable `(python +lsp +conda +pyright)` in `~/.doom.d/init.el' and run
;;    `doom sync' in terminal.
;;
;; 2. Language Server & Tooling (Pyright & Ruff):
;;    Pyright powers Microsoft's Pylance in VS Code and Antigravity.
;;    Install in your Anaconda base or virtual environment:
;;      $ conda activate base
;;      $ pip install pyright ruff
;;    Ensure executables are visible on PATH (or symlinked into ~/.local/bin):
;;      $ ln -sf ~/anaconda3/bin/pyright* ~/.local/bin/
;;      $ ln -sf ~/anaconda3/bin/ruff ~/.local/bin/
;;
;; 3. Python 3.13+ REPL Compatibility:
;;    Python 3.13+ defaults to `PyREPL', which can cause conflicts with Emacs
;;    comint buffers. `PYTHON_BASIC_REPL=1' is set below to ensure smooth REPL.
;;
;; 4. Interactive REPL Workflow (matching VS Code Shift+Enter / Smart Send):
;;    - `C-<return>' / `S-<return>' : Smart send block/paragraph & step forward.
;;    - `M-<return>'               : Send strictly current line & step.
;;    - `SPC m z' / `, z'          : Switch between editor and Python REPL.
;;    - `SPC m p' / `, p'          : Send current block/paragraph.
;;    - `SPC m l' / `, l'          : Send single line.
;;    - `SPC m r' / `, r'          : Send selected region.
;;    - `SPC m b' / `, b'          : Send entire buffer.
;;    - `SPC m f' / `, f'          : Send current function (`def`).
;;    - `M-x conda-env-activate'   : Switch Conda virtual environment.
;;    - `M-x lsp-workspace-restart': Restart LSP server after switching envs.

(defvar wz-anaconda-root
  (cl-find-if #'file-directory-p
              (list (expand-file-name "~/anaconda3")
                    (expand-file-name "~/miniconda3")
                    (expand-file-name "~/.conda")))
  "Root directory for Anaconda or Miniconda installation if present.")

(defvar wz-anaconda-python
  (or (and wz-anaconda-root
           (let ((bin (expand-file-name "bin/python3" wz-anaconda-root)))
             (and (file-executable-p bin) bin)))
      (executable-find "python3")
      (executable-find "python")
      "python3")
  "Absolute path to the Python executable (Anaconda if present, or system python).")

(defvar wz-anaconda-python-bin "/bin/python3"
  "Relative Python executable path inside a conda environment.")

;; Ensure Anaconda bin directory is in exec-path and PATH for Emacs if it exists.
(when wz-anaconda-root
  (let ((anaconda-bin (expand-file-name "bin" wz-anaconda-root)))
    (when (file-directory-p anaconda-bin)
      (add-to-list 'exec-path anaconda-bin)
      (setenv "PATH" (concat anaconda-bin ":" (getenv "PATH"))))))

(after! python
  ;; 1. Force interactive mode to ensure echo.
  (setq python-shell-interpreter-args "-i")
  ;; 2. Fix "TERM=dumb" warning and pyrepl error (Python 3.13+).
  ;; This tells Python not to try to be "smart" inside Emacs.
  (setenv "PYTHON_BASIC_REPL" "1")
  ;; ;; Optional: Ensure Python ignores system environment variables
  ;; ;; that may force colored/complex terminals.
  ;; (setenv "TERM" "dumb")

  ;; Set the default Anaconda interpreter.
  (setq python-shell-interpreter wz-anaconda-python))

(add-hook! python-mode
           ;; Local variable settings
  (setq-local lsp-diagnostics-provider :none
              comment-add 0)
  ;; Enable/Disable Minor Modes.
  (flycheck-mode -1))

;;---- Python REPL Auto-scroll Configuration ---------------------------
(add-hook 'inferior-python-mode-hook
          (lambda ()
            ;; Move cursor to end when sending new commands.
            (setq-local comint-scroll-to-bottom-on-input t)
            ;; Move cursor to end when Python returns text.
            (setq-local comint-scroll-to-bottom-on-output t)
            ;; Ensure point (cursor) follows scrolling.
            (setq-local comint-move-point-for-output t)))

;;----- Conda & LSP Integration ----------------------------------------
(use-package! conda
  :when wz-anaconda-root
  :init
  ;; Paths to your Anaconda installation.
  (setq conda-anaconda-home wz-anaconda-root
        conda-env-home-directory wz-anaconda-root)
  :config
  ;; Ativa o ambiente Conda automaticamente APENAS para arquivos Python
  (add-hook 'python-mode-hook #'conda-env-activate-for-buffer)
  ;; Critical Synchronization: When switching Conda environments, the LSP
  ;; and Python Interpreter must update.
  (add-hook 'conda-postactivate-hook
            (lambda ()
              ;; Update interpreter to the new environment's Python.
              (setq python-shell-interpreter
                    (concat conda-env-current-path wz-anaconda-python-bin))
              ;; Restart LSP to read the new environment's libraries.
              (lsp-workspace-restart))))

;;----- Python Interface Adjustments -----------------------------------
(after! lsp-pyright
  ;; Pyright server settings (Microsoft/VS Code default).
  (setq lsp-pyright-python-executable-cmd wz-anaconda-python)
  (setq lsp-pyright-multi-root nil))


;;----------------------------------------------------------------------
;; 7. AI & Code Assistance
;;----------------------------------------------------------------------

;;--- GPTel ------------------------------------------------------------
;; ~/.bashrc: export OPENAI_API_KEY_GPTEL="your_api_key_here"
;; (use-package! gptel
;;   :config
;;   (setq! gptel-api-key (getenv "OPENAI_API_KEY_GPTEL")))

;;--- Copilot ----------------------------------------------------------
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("TAB"   . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-n"   . 'copilot-next-completion)
              ("C-p"   . 'copilot-previous-completion))
  :config
  (customize-set-variable 'copilot-enable-predicates nil)
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(markdown-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

;; ;;--- Codeium ----------------------------------------------------------
;; (use-package! codeium
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   :config
;;   (setq use-dialog-box nil)
;;   (setq codeium-mode-line-enable
;;         (lambda (api) (not (memq api '(CancelRequest
;;                                        Heartbeat
;;                                        AcceptCompletion)))))
;;   (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
;;   (setq codeium-api-enabled
;;         (lambda (api)
;;           (memq api '(GetCompletions
;;                       Heartbeat
;;                       CancelRequest
;;                       GetAuthToken
;;                       RegisterUser
;;                       auth-redirect
;;                       AcceptCompletion))))
;;   ;; Optimization functions
;;   (defun my-codeium/document/text ()
;;     (buffer-substring-no-properties
;;      (max (- (point) 3000) (point-min))
;;      (min (+ (point) 1000) (point-max))))
;;   (defun my-codeium/document/cursor_offset ()
;;     (codeium-utf8-byte-length
;;      (buffer-substring-no-properties
;;       (max (- (point) 3000) (point-min)) (point))))
;;   (setq codeium/document/text 'my-codeium/document/text)
;;   (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;;----------------------------------------------------------------------
;; 8. Syntax Highlighting Extras
;;----------------------------------------------------------------------

;; Custom Keyword Highlighting (e.g. @walmes, @param)
(dolist (mode '(fundamental-mode emacs-lisp-mode lisp-mode org-mode
                shell-mode sh-mode ess-mode ess-r-mode polymode-mode
                python-mode markdown-mode latex-mode TeX-mode
                prog-mode web-mode html-mode css-mode yaml-mode
                js-mode))
  (add-hook! mode
    (lambda ()
      (setq font-lock-keywords-case-fold-search t)
      (font-lock-add-keywords
       nil ; specific to current buffer via hook
       '(("\\(^\\|[[:space:]]\\)@[[:alnum:]_.]+\\>"
          0 'font-lock-function-name-face t))))))

;; HL-TODO
(use-package! hl-todo
  :bind
  ("C-c l m" . hl-todo-previous)
  ("C-c l n" . hl-todo-next)
  :config
  (global-hl-todo-mode t)

  ;; High-contrast solid badge for personal WALMES marker (warning fill, canvas text)
  (defface wz-hl-todo-walmes
    '((t :inherit warning
         :inverse-video t
        ;; :box (:line-width -1 :style nil)
         :weight bold))
    "High-contrast badge face for WALMES keyword in hl-todo, dynamically derived from theme."
    :group 'hl-todo)

  (setq hl-todo-keyword-faces
        '(;; 1. Identity / Personal Marker
          ("WALMES"      . wz-hl-todo-walmes)

          ;; 2. Critical Errors / Blockers (Red)
          ("ERROR"       . error)
          ("BUG"         . error)
          ("FAIL"        . error)
          ("DANGER"      . error)
          ("STOP"        . error)
          ("FIXME"       . error)

          ;; 3. Warnings / Review / Attention / Performance (Yellow / Amber)
          ("WARNING"     . warning)
          ("CAUTION"     . warning)
          ("ATTENTION"   . warning)
          ("IMPORTANT"   . warning)
          ("REVIEW"      . warning)
          ("PROBLEM"     . warning)
          ("OBS"         . warning)
          ("SLOW"        . warning)

          ;; 4. To Do / Work In Progress (Orange / Bold)
          ("TODO"        warning bold)
          ("WIP"         warning bold)
          ("HACK"        warning bold)
          ("DEBUG"       warning bold)

          ;; 5. Educational / Teaching / Theory (Cyan / Constant Face)
          ("EXERCISE"    . font-lock-constant-face)
          ("QUESTION"    . font-lock-constant-face)
          ("THEORY"      . font-lock-constant-face)
          ("IMPROVE"     . font-lock-constant-face)

          ;; 6. Tips & Informational Notes (Doc Face / Soft Blue)
          ("NOTE"        . font-lock-keyword-face)
          ("INFO"        . font-lock-keyword-face)
          ("TIP"         . font-lock-keyword-face)
          ("TRICK"       . font-lock-keyword-face)
          ("EXPLANATION" . font-lock-keyword-face)

          ;; 7. Success / Completed (Green)
          ("DONE"        . success)
          ("SOLVED"      . success)
          ("OKAY"        . success)
          ("BONUS"       . success))))

;; HL-Prog-Extra
(use-package! hl-prog-extra
  :commands (hl-prog-extra-mode)
  :config
  (setq hl-prog-extra-list
        (list
         ;; To highlight R packages: {tidyverse}.
         '("{[^{]+}" 1 comment-only font-lock-keyword-face)
         ;; To highlight code: `code`.
         '("`[^`]+`" 1 comment-only font-lock-constant-face)
         ;; Match URLs: http://xyz.com.
         '("\\<https?://[^[:blank:]]*" 1 comment success)))
  :init
  (add-hook 'ess-mode-hook #'hl-prog-extra-mode))

;; End of config.el ----------------------------------------------------
