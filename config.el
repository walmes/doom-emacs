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
  '(fill-column-indicator
    :foreground "#3f444a"
    :background nil
    :weight light)
  '(whitespace-line
    :background nil
    :foreground "#f0c674"
    :weight normal))

;; Whitespace Configuration for Programming & Config Modes
(setq-hook! '(prog-mode-hook conf-mode-hook)
  display-fill-column-indicator-column fill-column
  whitespace-line-column fill-column
  whitespace-style '(face lines-tail trailing tabs empty)
  ;; whitespace-style '(face trailing tabs empty)
  )

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

;;--- Org Mode & Presentation ------------------------------------------
(use-package! visual-fill-column
  :config
  (setq visual-fill-column-center-text t))

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
      (require 'ess-site)
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
;; NOTE: Doom has a `(python +lsp +conda +pyright)` module that handles
;; much of this. Please ensure you have enabled it in `init.el`.

;;$ source ~/anaconda3/bin/activate
;;$ conda activate base
;;$ pip install pyright

;; Install LSP: `M-x lsp-install-server'.
;; Activate a conda virtual environment: `M-x conda-env-activate'.
;; Open Python REPL: `M-x run-python'.

(defvar wz-anaconda-root (expand-file-name "~/anaconda3")
  "Root directory for the Anaconda installation.")
(defvar wz-anaconda-python (expand-file-name "~/anaconda3/bin/python3")
  "Absolute path to the Anaconda Python executable.")
(defvar wz-anaconda-python-bin "/bin/python3"
  "Relative Python executable path inside a conda environment.")

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
  (setq python-shell-interpreter wz-anaconda-python)

  ;; Function: Send line and step to next (R-style Ctrl+Enter).
  ;; (defun wz-python-send-line-and-step ()
  ;;   "Send current line to Python shell and move to next."
  ;;   (interactive)
  ;;   (python-shell-send-statement)
  ;;   (forward-line 1))

  (defun wz-python-send-line-and-step ()
    "Send line to Python. Open process if it doesn't exist, split window
     and ensure correct initialization."
    (interactive)
    (let ((proc (python-shell-get-process))
          (cmd (python-shell-calculate-command))) ; Use Anaconda path defined in config
      (unless proc
        (save-selected-window
          ;; The 't' does the split. 'save-selected-window' returns focus.
          (run-python cmd nil t)
          (setq proc (python-shell-get-process))
          ;; Synchronization: Wait 0.5s for Python to load
          ;; Emacs internal functions (__PYTHON_EL_eval)
          (accept-process-output proc 0.5)))

      ;; Send code and jump to next line.
      (python-shell-send-statement)
      (forward-line 1)))

  ;; Key Mapping (For Leader/Emacs-style users).
  (map! :map python-mode-map
        ;; Universal execution shortcut.
        "C-<return>" #'wz-python-send-line-and-step

        ;; Shortcuts with Local-Leader (C-c C-z, C-c C-r, etc).
        :localleader
        "z" #'python-shell-switch-to-shell  ; Switch between script and terminal.
        "r" #'python-shell-send-region      ; Send selected block.
        "b" #'python-shell-send-buffer      ; Send entire file.
        "f" #'python-shell-send-defun)      ; Send current function.
  )

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
  :init
  ;; Paths to your Anaconda installation.
  (setq conda-anaconda-home wz-anaconda-root
        conda-env-home-directory wz-anaconda-root)
  :config
  ;; Activate environment automatically if there's an environment.yml in the project.
  (conda-env-autoactivate-mode t)
  ;; Critical Synchronization: When switching Conda environments, the LSP
  ;; and Python Interpreter must update.
  (add-hook 'conda-postactivate-hook
            (lambda ()
              ;; Update interpreter to the new environment's Python.
              (setq python-shell-interpreter
                    (concat conda-env-current-path wz-anaconda-python-bin))
              ;; Restart LSP to read the new environment's libraries.
              ;; (lsp-restart-workspace)
              (sp-workspace-restart))))

;;----- Python Interface Adjustments -----------------------------------
(after! lsp-pyright
  ;; Pyright server settings (Microsoft/VS Code default).
  (setq lsp-pyright-python-executable-cmd wz-anaconda-python)
  (setq lsp-pyright-multi-root nil))


;;----------------------------------------------------------------------
;; 7. AI & Code Assistance
;;----------------------------------------------------------------------

;;--- GPTel ------------------------------------------------------------
(use-package! gptel
  :config
  (setq! gptel-api-key (getenv "OPENAI_API_KEY_GPTEL")))

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
  (setq hl-todo-keyword-faces
        (append hl-todo-keyword-faces
                '(("IMPROVE"     . font-lock-constant-face)
                  ("QUESTION"    . font-lock-constant-face)
                  ("EXPLANATION" . font-lock-constant-face)
                  ("THEORY"      . font-lock-constant-face)
                  ("DESCRIPTION" . font-lock-keyword-face)
                  ("COMMENT"     . font-lock-keyword-face)
                  ("TIP"         . font-lock-keyword-face)
                  ("TRICK"       . font-lock-keyword-face)
                  ("INFO"        . font-lock-keyword-face)
                  ("INFORMATION" . font-lock-keyword-face)
                  ("DANGER"      . error)
                  ("STOP"        . error)
                  ("FAIL"        . error)
                  ("WARNING"     . error)
                  ("ERROR"       . error)
                  ("BUG"         . error)
                  ("DEBUG"       . warning)
                  ("IMPORTANT"   . warning)
                  ("ATTENTION"   . warning)
                  ("CAUTION"     . warning)
                  ("REVIEW"      . warning)
                  ("OBS"         . warning)
                  ("PROBLEM"     . warning)
                  ("DISCLAIMER"  . warning)
                  ("EXERCISE"    . warning)
                  ("BONUS"       . success)
                  ("DONE"        . success)
                  ("OKAY"        . success)
                  ("GOOD"        . success)
                  ("SOLVED"      . success)
                  ("OPTIONAL"    . success)))))

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
