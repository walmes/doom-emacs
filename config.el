;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;======================================================================
;; Configuration file to Doom Emacs (>=26.3) by Walmes Zeviani.
;;
;; This file is hosted at https://github.com/walmes/doom-emacs.
;;
;; Almost all the content available here was obtained/inspired by
;; queries on the internet. Please, send questions, problems and/or
;; suggestions as an issue on GitHub project of this file.
;;======================================================================

;; Some configurations to get inspired.
;; https://zzamboni.org/post/my-doom-emacs-configuration-with-commentary/
;; https://dotdoom.rgoswami.me/config.html
;; https://emacs.zdx.cat/

;;----------------------------------------------------------------------
;; http://www.emacswiki.org/wiki/EmacsNiftyTricks
;; “I’ve used Emacs for many years now, but have never reached its
;;    maximum potential.” -- Anon.
;;
;; http://www.mygooglest.com/fni/dot-emacs.html
;; “Show me your ~/.emacs and I will tell
;;    you who you are.” -- Bogdan Maryniuk.
;;
;; https://www.emacswiki.org/emacs/EmacsKoans
;; “-- Master, does Emacs have buddha-nature?
;;  -- I can't se why not, it has everything else.”

;;----------------------------------------------------------------------
;; Place your private configuration here! Remember, you do not need to
;; run 'doom sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration,
;; email clients, file templates and snippets.
(setq user-full-name "Walmes Zeviani"
      user-mail-address "walmeszeviani")

;; Doom exposes five (optional) variables for controlling fonts in Doom.
;; Here are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or
;; xlfd font string. You generally only need these two:
;; (setq ;; doom-font (font-spec :family "Spline Sans Mono" :weight 'light)
;;       doom-font (font-spec :family "Fira Mono" :weight 'light)
;;       ;; doom-big-font (font-spec :family "Fira Mono" :weight 'normal :size 21)
;;       doom-big-font (font-spec :size 21)
;;       ;; doom-variable-pitch-font (font-spec :family "Montserrat" :size 13)
;;       )

;; NOTE: to list all fonts available in your system, run on terminal.
;;   fc-list | grep FiraCode
;; (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 128 :width normal)
;; (:family "JetBrains Mono" :foundry "JB" :slant normal :weight extra-light :height 128 :width normal)

;; (defvar doom-big-font-increment 3)

;; There are two ways to load a theme. Both assume the theme is
;; installed and available. You can either set `doom-theme' or manually
;; load a theme with the `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-bluloco-dark)
;; (setq doom-theme 'doom-horizon)
;; (setq doom-theme 'doom-city-lights)
;; (setq doom-theme 'doom-tokyo-night)
;; (setq doom-theme 'doom-ayu-dark)

;; (use-package! spacemacs-theme
;;   :init (load-theme 'spacemacs-dark t))

;; If you use `org' and don't want your org files in the default
;; location below, change `org-directory'. It must be set before org
;; loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil',
;; line numbers are disabled. For relative line numbers, set this to
;; `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you
;; configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path',
;;   relative to this file. Emacs searches the `load-path' when you load
;;   packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the
;; cursor over the highlighted symbol at press 'K' (non-evil users must
;; press 'C-c c k'). This will open documentation for it, including
;; demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and
;; see how they are implemented.

;;----------------------------------------------------------------------
;; Basic definitions.
;;----------------------------------------------------------------------

(global-hl-line-mode 1)             ;; Highlight the cursor line.
(visual-line-mode 1)                ;; Screen lines, not logical lines.
(show-paren-mode 1)                 ;; Highlight matching pairs.
(delete-selection-mode 1)           ;; Allows delete region.
(recentf-mode 1)                    ;; List of recently opened files.
(global-auto-revert-mode 1)         ;; Refresh buffer if file changes.

;; (global-flycheck-mode -1)           ;; Turn off Flycheck.

(setq column-number-mode t)         ;; Show cursor position.
(setq auto-save-default nil)        ;; Turn off #autosave#.
(setq make-backup-files nil)        ;; Turn off backup~.
(setq comment-empty-lines t)        ;; Comment even in empty lines.
(setq select-enable-clipboard t)    ;; Allow shared transfer area.
(setq tab-always-indent t)
(setq-default indent-tabs-mode nil) ;; Spaces to indent.
(setq-default fill-column 72)       ;; Column width.

;; Highlight whitespace.
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-line-column fill-column)
(setq whitespace-style '(face lines-tail trailing tabs empty))

;; (setq doom-variable-pitch-font
;;       (font-spec :family "Noto Sans" :size 12))
(setq doom-themes-treemacs-enable-variable-pitch nil)

;; When scrolling with the cursor, show 4 lines above/below.
(setq scroll-margin 5)

(dolist
    (mode '(messages-buffer-mode-hook
            comint-mode-hook
            term-mode-hook
            erc-mode-hook
            inferior-ess-mode-hook
            eshell-mode-hook
            inferior-python-mode-hook))
  (set (make-local-variable 'scroll-margin) 0)
  )

;; Let the desktop background show through.
;; (set-frame-parameter (selected-frame) 'alpha '(97 . 100))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;----------------------------------------------------------------------
;; Key bindings.
;;----------------------------------------------------------------------

;; C-z to 'undo, the default is C-/.
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)

;; Uses C-/ to complete paths.
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-/") 'company-files)

;; M-. to (un)comment paragraph.
(global-set-key [?\M-.] (kbd "M-h M-; M-}"))

;; M-+ to indent paragraph.
(global-set-key [?\M-+] (kbd "M-h C-M-\\"))

;; "C-~" to keep one white space between objects around point.
(global-set-key (kbd "<C-dead-tilde>") 'fixup-whitespace)

;; "M-~" to joint lines.
(global-set-key (kbd "<M-dead-tilde>") 'delete-indentation)

;; S-F11 and S-F12 to show/hide menu bar and tool bar.
(global-set-key (kbd "<S-f11>") 'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "<S-f12>") 'toggle-tool-bar-mode-from-frame)

;; (global-auto-revert-mode 1)
(global-set-key [f5] 'revert-buffer)

;; ;; Navigation in balanced expressions.
;; (dolist (mode '(ess-mode-hook lisp-mode-hook))
;;   (add-hook mode
;;             '(lambda ()
;;                (global-set-key (kbd "<M-right>")  'forward-sexp)
;;                (global-set-key (kbd "<M-left>")   'bakward-sexp)
;;                (global-set-key (kbd "<M-down>")   'forward-list)
;;                (global-set-key (kbd "<M-up>")     'backward-list)
;;                (global-set-key (kbd "<M-S-up>")   'backward-up-list)
;;                (global-set-key (kbd "<M-S-down>") 'down-list))))

;;----------------------------------------------------------------------
;; My functions.
;;----------------------------------------------------------------------

;; Add directory with supplementary configuration files.
;; (add-to-list 'load-path "~/.doom.d/")
(add-load-path! "~/.doom.d/")

;; Byte compile file.
;; (byte-compile-file "~/.doom.d/funcs.el")

;; Load my functions.
(require 'funcs)

;;----------------------------------------------------------------------
;; Configures `company'.

(use-package! company
  :bind
  ("C-*" . company-complete))

;;----------------------------------------------------------------------
;; Magit.

(use-package! magit
  :bind
  ("C-c g" . magit-status))

;;----------------------------------------------------------------------
;; Bookmark-plus.

;; Byte compile file. Faster load and execution.
;; http://ergoemacs.org/emacs/emacs_byte_compile.html
;; (byte-recompile-directory "~/.emacs.d/elpa/bookmark+" 0 t)

;; To avoid the error `(void-function org-link-set-parameters)'.
(use-package! ol)

(use-package! bookmark+
  :init
  (when (file-exists-p "~/Dropbox/bookmarks")
    (setq bookmark-default-file "~/Dropbox/bookmarks"
          bookmark-save-flag 1))
  :config
  ;; ATTENTION: for some unknown reason, the keymap must be defined in
  ;; `:config' because in `:bind' the bookmark list buffer have a
  ;; different appearance.
  (progn
    ;; Create an autonamed bookmark.
    (global-set-key (kbd "<C-f3>")
                    'bmkp-toggle-autonamed-bookmark-set/delete)
    ;; Go to the next bookmark in file.
    (global-set-key (kbd "<f3>")
                    'bmkp-next-bookmark-this-file/buffer-repeat)
    ;; Go to the previous bookmark in file.
    (global-set-key (kbd "<f4>")
                    'bmkp-previous-bookmark-this-file/buffer-repeat)
    ;; Toggle temporary/permanent bookmark.
    (global-set-key (kbd "<S-f3>")
                    'bmkp-toggle-temporary-bookmark)
    ))

;;----------------------------------------------------------------------
;; Visible bookmarks. Easy movement.
;; https://marmalade-repo.org/packages/bm

(use-package! bm
  :config
  (setq bm-marker 'bm-marker-left
        bm-highlight-style 'bm-highlight-only-fringe)
  :bind
  (("<C-f2>" . bm-toggle)
   ("<f2>"   . bm-next)
   ("<S-f2>" . bm-previous)))

;;----------------------------------------------------------------------
;; Folding code blocks based on indentation.
;; git clone https://github.com/zenozeng/yafolding.el.git

(use-package! yafolding
  :bind
  (("C-{" . yafolding-hide-parent-element)
   ("C-}" . yafolding-toggle-element)))

;;----------------------------------------------------------------------
;; Snippets.
;; https://joaotavora.github.io/yasnippet/snippet-development.html
;; http://pragmaticemacs.com/emacs/smart-text-templates-with-yasnippet/

(use-package! yasnippet
  :config
  (yas-global-mode 1))

;;----------------------------------------------------------------------
;; Web-mode.

(use-package! web-mode
  :config
  (progn
    (add-hook 'web-mode-hook
              '(lambda ()
                 (setq web-mode-markup-indent-offset 4)
                 ))
    ))

;;----------------------------------------------------------------------
;; Treemacs and icons.

(use-package! treemacs
  :config
  (setq treemacs-is-never-other-window nil))

(use-package! treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package! nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

;;----------------------------------------------------------------------
;; Neotree.

;; (after! doom-themes
;;   (remove-hook 'doom-load-theme-hook #'doom-themes-neotree-config))
;;
;; (use-package! neotree
;;   :config
;;   (setq neo-theme 'icons)
;;   ;; (setq doom-neotree-file-icons t)
;;   ;; (setq doom-themes-neotree-file-icons t)
;;   ;; (setq doom-neotree-enable-variable-pitch nil)
;;   )

;;----------------------------------------------------------------------
;; Dired sidebar.
;; https://github.com/jojojames/dired-sidebar

(use-package! dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  ;; :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'ascii)
  ;; (setq dired-sidebar-theme 'nerd)
  ;; (setq dired-sidebar-theme 'icons)
  ;; (setq dired-sidebar-theme 'none)
  ;; (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font nil)
)

;;----------------------------------------------------------------------
;; imenu-list

(use-package! imenu-list
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t)
  (setq imenu-list-size 0.2)
  (setq imenu-list-position 'left)
  :bind
  ("<f12>" . imenu-list-smart-toggle)
)

;;----------------------------------------------------------------------
;; Uses `M-x screeshot' to take a screenshot of a region or buffer.
;; https://www.emacswiki.org/emacs/ScreenShot
;; https://github.com/tecosaur/screenshot

(use-package! screenshot
  :config
  (setq screenshot-schemes
        '(("current-directory" :dir default-directory)))
  (setq screenshot-default-scheme "current-directory"))

;;----------------------------------------------------------------------
;; Org Mode.

;; (use-package! org
;;   :config
;;   (progn
;;     (require 'orgalist)
;;     (orgalist-mode t)
;;     )
;;   )

;;----------------------------------------------------------------------
;; Org Present.
;; https://elpa.nongnu.org/nongnu/org-present.html
;; TODO: import configuration at https://systemcrafters.net/emacs-tips/presentations-with-org-present/

(use-package! visual-fill-column
  :config
  ;; Configure fill width.
  (setq visual-fill-column-center-text t)
  ;; (setq visual-fill-column-width 150
  ;;       visual-fill-column-center-text t)
  )

;; Unfold the current entry and show only direct subheadings of the
;; slide but don't expand them.
;; (defun my/org-present-prepare-slide (buffer-name heading)
;;   ;; Show only top-level headlines
;;   (org-overview)
;;   ;; Unfold the current entry
;;   (org-show-entry)
;;   ;; Show only direct subheadings of the slide but don't expand them
;;   (org-show-children))
;; (add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

;; Minimal configuration.
(use-package! org-present
  :config
  (progn
    (add-hook 'org-present-mode-hook
              '(lambda ()
                 (org-display-inline-images)
                 (setq header-line-format " ")
                 (visual-fill-column-mode t)
                 (visual-line-mode t)
                 )
              )
    (add-hook 'org-present-mode-quit-hook
              (lambda ()
                (org-remove-inline-images)
                (setq header-line-format nil)
                (visual-fill-column-mode nil)
                (visual-line-mode nil)
                )
              )
    )
  )

;;----------------------------------------------------------------------
;; Perfect Margin.
;; https://github.com/mpwang/perfect-margin

(use-package! perfect-margin
  :config
  (after! doom-modeline
    (setq mode-line-right-align-edge 'right-fringe))
  (after! minimap
    ;; if you use (vc-gutter +pretty)
    ;; and theme is causing "Invalid face attribute :foreground nil"
    ;; (setq minimap-highlight-line nil)
    (setq minimap-width-fraction 0.08))
  ;; (setq perfect-margin-only-set-left-margin t)
  ;; (perfect-margin-mode t)
  )

;;----------------------------------------------------------------------
;; Solve problem about text face related to tibble, rlang, messages and
;; warnings.
;; https://github.com/emacs-ess/ESS/issues/1193

(use-package! xterm-color
  :init
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'inferior-ess-mode-hook
            (lambda () (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter nil t)))
  :config
  (setq xterm-color-use-bold t))

;; ;; https://stackoverflow.com/a/72132446
;; (defun xterm-color-colorize-shell-command-output ()
;;   "Colorize `shell-command' output."
;;   (let ((bufs
;;          (seq-remove
;;           (lambda (x)
;;             (not (or (string-prefix-p " *Echo Area" (buffer-name x))
;;                      (string-prefix-p "*Shell Command" (buffer-name x)))))
;;           (buffer-list))))
;;     (dolist (buf bufs)
;;       (with-current-buffer buf
;;         (xterm-color-colorize-buffer)))))
;; (defun xterm-color-colorize-shell-command-output-advice (proc &rest rest)
;;   (xterm-color-colorize-shell-command-output))
;; (advice-add 'shell-command :after #'xterm-color-colorize-shell-command-output-advice)
;; ;; (advice-remove 'shell-command #'xterm-color-colorize-shell-command-output-advice)

;;----------------------------------------------------------------------
;; MarkDown configuration.

;; OrgStruct funcionally was removed from Org in version 9.2. The last
;; version with it is 9.1.14.
;; https://github.com/bzg/org-mode/releases/tag/release_9.1.14

;; ATTENTION: lists only works with {1., a., -, +}. So, {1), *} are not
;; recognized by `orgalist' package.
(use-package! markdown-mode
  :config
  (progn
    (require 'orgalist)
    (orgalist-mode t)
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (global-set-key (kbd "C-c *")
                                 'orgalist-cycle-bullet)))
    (require 'imenu-list)
    (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
    (add-hook 'markdown-mode-hook
              '(lambda ()
                 (global-set-key (kbd "<f10>")
                                 'imenu-list-smart-toggle)))
    ))

;;----------------------------------------------------------------------
;; essh.el - ESS like shell mode. To eval line/regions in Emacs shell.
;; https://www.emacswiki.org/emacs/download/essh.el

;; Download.
(progn
  (when (not (file-exists-p "~/.doom.d/essh.el"))
    (url-copy-file
     "https://www.emacswiki.org/emacs/download/essh.el"
     "~/.doom.d/essh.el")))

;; Bite compile.
(when (not (file-exists-p "~/.doom.d/essh.elc"))
  (byte-compile-file "~/.doom.d/essh.el"))

(use-package! essh
  :config
  (add-hook
   'sh-mode-hook
   '(lambda ()
      (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
      (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
      (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
      (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
      (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
      (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))))

;; (defun my-shell-mode-setup-imenu ()
;;   (setq imenu-generic-expression (append '(("Variables" "^\\([A-Z_]+\\)=.*" 1))
;;                                          (nthcdr 1 (car sh-imenu-generic-expression)))))
;; (add-hook 'sh-mode-hook 'my-shell-mode-setup-imenu)

;; (defun my-block-mode-setup-imenu ()
;;   (setq imenu-generic-expression (append '(("Blocks" "^# \\(.*\\) ---+$" 1))
;;                                         (nthcdr 1 (car sh-imenu-generic-expression)))))
;; (add-hook 'sh-mode-hook 'my-block-mode-setup-imenu)

(defun my-block-mode-setup-imenu ()
  (setq imenu-generic-expression
        (append '(("Blocks" "^# \\(.\\{1,15\\}\\)[^-]* ---+$" 1))
                (nthcdr 1 (car sh-imenu-generic-expression))
                )
        )
  )
(add-hook 'sh-mode-hook 'my-block-mode-setup-imenu)

;;----------------------------------------------------------------------
;; hi-lock.el - Highlight patterns in buffer.
;; https://www.emacswiki.org/emacs/HiLock
;; https://emacs.stackexchange.com/questions/15025/highlighting-automatically-on-file-open

;; Download.
(progn
  (when (not (file-exists-p "~/.doom.d/hi-lock.el"))
    (url-copy-file
     "http://web.mit.edu/Emacs/source/emacs/lisp/hi-lock.el"
     "~/.doom.d/hi-lock.el")))

;; Bite compile.
(when (not (file-exists-p "~/.doom.d/hi-lock.elc"))
  (byte-compile-file "~/.doom.d/hi-lock.el"))

;; M-s h r   highlight-regexp                    ;; <-- Create.
;; M-s h w   hi-lock-write-interactive-patterns  ;; <-- Save in buffer.
;; M-s h u   unhighlight-regexp
;; M-s h p   highlight-phrase
;; M-s h l   highlight-lines-matching-regexp
;; M-s h f   hi-lock-find-patterns
(use-package! hi-lock)

;; Example of syntax resulted.
;; Hi-lock: (("`.*`" (0 'hi-blue-b t)))
;; Hi-lock: (("{.*}" (0 'hi-blue-b t)))
;; QUESTION: activate this for some modes.

;;----------------------------------------------------------------------
;; TODO Read this
;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

;; ;; https://github.com/emacs-lsp/lsp-mode/issues/1383
;; (use-package! lsp-mode
;;   :config
;;   (setq lsp-enable-snippet t
;;         lsp-prefer-flymake nil)
;;   (setq lsp-eldoc-hook '(lsp-hover))
;;   (remove-hook 'lsp-eldoc-hook 'lsp-document-highlight))

;; (setq lsp-restart 'ignore)
;; (setq lsp-restart 'auto-restart)
;; (setq lsp-keep-workspace-alive nil)

;; TODO Read all this. The code below was inspired here:
;; https://awesomeopensource.com/project/MatthewZMD/.emacs.d

;; https://github.com/emacs-lsp/lsp-ui
(use-package! lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-headerline-breadcrumb-enable t)
  :config
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (map! :map lsp-ui-mode-map
        ;; ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ;; ([remap xref-find-references] . lsp-ui-peek-find-references)
        "C-c u" #'lsp-ui-doc-show
        "M-i" #'lsp-ui-doc-focus-frame
        )
  (map! :map lsp-mode-map
        "M-n" #'forward-paragraph
        "M-p" #'backward-paragraph
        )
  ;; Use lsp-ui-doc-webkit only in GUI
  ;; (if (display-graphic-p)
  ;;     (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice
      lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

;; TODO Doom-emacs configuration. How to use `use-package!' fields.
;; https://tecosaur.github.io/emacs-config/config.html

;; TODO Take this project as a referece of Doom-Emacs.
;; https://dotdoom.rgoswami.me/config.html

;;----------------------------------------------------------------------
;; lsp-treemacs
;; https://github.com/emacs-lsp/lsp-treemacs

(use-package! lsp-treemacs
  :bind
  ("C-<f8>" . lsp-treemacs-symbols-toggle)
  ("<f8>" . lsp-ui-imenu-toggle)
  )

;;----------------------------------------------------------------------
;; ESS - Emacs Speaks Statistics.
;; http://ess.r-project.org/

;; Automatically connecting to remote R sessions in Emacs using ESS
;; https://www.dcalacci.net/2018/remote-ess/

;; (add-hook 'ess-mode-hook #'lsp-deferred)

(use-package! ess
  ;; (ess :variables ess-r-backend 'lsp)
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
    ;; (setq lsp-diagnostics-provider :none)
    (setq ;; ess-r-backend 'lsp
          ;; ess-style 'RStudio
          ess-use-flymake nil)
    )
  :bind
  (("C-S-<f5>" . ess-eval-chunk)
   ("C-S-<f6>" . ess-eval-chunk-and-step)
   ("C-S-<f7>" . ess-noweb-next-code-chunk)
   ("C-S-<f8>" . ess-noweb-previous-code-chunk)
   ("C-S-<f9>" . ess-noweb-goto-chunk)
   ;; Native pipe.
   ("C-|" . " |>"))
  :config
  ;; Script and console font lock highlight.
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
      ;;-------------------------------------
      (require 'ess-site)
      (require 'ess-view-data)
      ;; (setq ess-view-data-mode t)
      (flycheck-mode -1)        ;; Disable flycheck/lintr.
      (setq ess-smart-operators t)
      (setq-local comment-add 0) ;; Single # as default.
      ;; (ess-toggle-underscore nil)
      ;;
      ;; https://stackoverflow.com/questions/7502540/make-emacs-ess-follow-r-style-guide
      ;; (ess-set-style 'C++)
      (ess-set-style 'RStudio)
      ;; (setq ess-offset-arguments 'prev-line)
      ;; (set 'ess-arg-function-offset t)
      ;;
      ;;-------------------------------------
      ;; LSP.
      ;; https://github.com/emacs-lsp/lsp-ui/issues/367
      (setq lsp-enable-symbol-highlighting nil)
      (setq lsp-signature-auto-activate nil)
      (setq lsp-ui-doc-enable nil)
      (setq lsp-diagnostics-provider :none)
      (setq lsp-restart 'ignore)
      ;; (setq lsp-enable-symbol-highlighting nil) ;; https://github.com/syl20bnr/spacemacs/issues/13934
      ;;
      ;; Company. ---------------------------
      ;; (company-mode 1)
      ;; (setq ess-use-company 'script-only)
      ;; `Alt + -'  to cycle `<- | <<- | = ...'.
      (define-key ess-mode-map [?\M--] 'ess-cycle-assign)
      (define-key ess-mode-map [S-f5] 'company-R-args)    ;; S-F5 do show ARGS.
      (define-key ess-mode-map [C-f5] 'company-R-objects) ;; C-F5 complete objects.
      )
   )
  ;; SOLVED: https://github.com/syl20bnr/spacemacs/issues/5395#issuecomment-297027630
  (add-hook
   'inferior-ess-mode-hook
   '(lambda ()
      (setq-local comint-use-prompt-regexp nil)
      (setq-local inhibit-field-text-motion nil)
      )
   )
  ;;-----------------------------------------
  (defadvice ess-eval-buffer (before really-eval-buffer compile activate)
    "Prevent call ess-eval-buffer by accident, frequently by
     hitting C-c C-b instead of C-c C-n."
    (if (yes-or-no-p
         (format "Are you sure you want to evaluate the %s buffer?"
                 buffer-file-name))
        (message "ess-eval-buffer started.")
      (error "ess-eval-buffer canceled!")))
  )

;;----------------------------------------------------------------------
;; Smart operators with electric spacing.
;; https://github.com/walmes/electric-spacing (fork).

;; Download.
(progn
  (when (not (file-exists-p "~/.doom.d/electric-spacing-r.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/walmes/electric-spacing/master/electric-spacing-r.el"
     "~/.doom.d/electric-spacing-r.el")))

;; Bite compile.
(when (not (file-exists-p "~/.doom.d/electric-spacing-r.elc"))
  (byte-compile-file "~/.doom.d/electric-spacing-r.el"))

(use-package! electric-spacing-r
  :config
  (add-hook 'ess-mode-hook #'electric-spacing-mode)
  (add-hook 'python-mode-hook #'electric-spacing-mode))

;----------------------------------------------------------------------
;; R+MarkDown extensions (emacs >= 24.3.1).
;; (IT MUST BE BEFORE LATEX EXTENSIONS.)

;; Based on:
;; https://github.com/fernandomayer/spacemacs/blob/master/private/polymode/packages.el#L13

;; `polymode' is included in `ess' module.
;; (use-package polymode
;;   :mode (("\\.Rmd" . Rmd-mode))
;;   :init
;;   (progn
;;     (defun Rmd-mode ()
;;       "ESS Markdown mode for Rmd files"
;;       (interactive)
;;       (require 'poly-R)
;;       (require 'poly-markdown)
;;       (R-mode)
;;       (poly-markdown+r-mode))
;;     ))
;; (use-package polymode
;;   :mode (("\\.Rnw" . Rnw-mode))
;;   :init
;;   (progn
;;     (defun Rnw-mode ()
;;       "ESS LaTeX mode for Rnw files"
;;       (interactive)
;;       (require 'poly-R)
;;       (require 'poly-noweb)
;;       (R-mode)
;;       (poly-noweb+r-mode))
;;     ))

;;----------------------------------------------------------------------
;; Quarto.

(use-package quarto-mode)

;;----------------------------------------------------------------------
;; Python as a IDE with REPL.

;; ATTENTION: adds to yout `~/.bachrc' file
;; $ export PATH="$HOME/anaconda3/bin:$PATH"

;; To install .NET and use MS Python Language Server (`mypyls').
;; https://dotnet.microsoft.com/en-us/download
;; $ sudo snap install --classic dotnet-sdk
;;
;; Install `mypyls'.
;; $ cd ~/Documents/
;; $ git clone https://github.com/Microsoft/python-language-server.git
;; $ cd python-language-server/src/LanguageServer/Impl
;; $ dotnet publish -c Release -r linux-x64
;; $ chmod a+x $(git rev-parse --show-toplevel)/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer
;;
;; Adds the following configuration at `config.el`.
;; (setq lsp-python-ms-executable
;;       "~/Documents/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")

(use-package! python
  ;; :hook
  ;; (python-mode . lsp-deferred)
  ;; :bind (:map python-mode-map
  ;;             ;; Use this if not usiung Elpy.
  ;;             ;; ("C-<return>" . python-shell-send-statement)
  ;;             )
  :config
  (setq python-indent-offset 4)
  ;; (setq python-shell-interpreter "/home/walmes/anaconda3/bin/python3") ;; ATTENTION: not use, cause conflict with jedi.
  ;; (define-key python-mode-map [S-f5] 'company-complete)
  ;; (define-key python-mode-map [S-f6] 'complete-symbol)
  ;; ATTENTION: Third party software installed apart.
  (setq lsp-python-ms-executable
        "~/Documents/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer")
  )

;; Elpy, the Emacs Python IDE. Elpy is an Emacs package to bring
;; powerful Python editing to Emacs. It combines and configures a number
;; of other packages, both written in Emacs Lisp as well as Python.
;; https://github.com/jorgenschaefer/elpy
;;
;; Main commands
;; C-c C-c: evaluates the buffer or region.
;; C-RET: evaluates the block.
;; C-c C-z: switches between your script and the interactive shell.
;; C-c C-d: displays documentation for the thing under cursor.
(use-package! elpy
  :init
  (elpy-enable)
  :config
  ;; Elpy will install RPC dependencies automatically.
  (setq elpy-rpc-python-command "/home/walmes/anaconda3/bin/python3")
  )

;; Check the backend enabled.
;; (describe-variable 'company-backends)

;; To list conda envs.
;;   cd anaconda
;;   source activate
;;   conda info --envs
;; (use-package pyvenv
;;   :ensure t
;;   :init
;;   (setenv "WORKON_HOME" "~/anaconda3"))

;; ATTENTION:
;; Choose an anaconda Env:
;;   M-x conda-env-activate base
;; Restart LSP:
;;   M-x lsp ...or... M-x lsp-restart-workspace

;; https://www.reddit.com/r/emacs/comments/hkshob/save_correct_condaenv_for_project/fwxty9v
;;
;; 1. Set `conda-project-env-name' as a directory local variable. (With
;;    projectile you could use the `projectile-edit-dir-locals'
;;    command.)
;;
;; 2. Use `conda-env-activate-for-buffer' to activate the environment
;;    set. (Or enable `conda-env-autoactivate-mode' to automatically
;;    activate it.)

;; A conda environment manager, assuming the use of Anaconda and the
;; `conda` tool. See https://github.com/necaris/conda.el for more
;; details. https://melpa.org/#/conda.
(use-package! conda
  :init
  ;; (message "HERE conda init")
  (setq conda-anaconda-home (expand-file-name "~/anaconda3"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda3"))
  :config
  ;; (message "HERE conda config")
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell)
  ;; (conda-env-activate 'getenv "CONDA_DEFAULT_ENV")
  (conda-env-autoactivate-mode t)
  )

;; Code navigation, documentation lookup and completion for Python.
;; https://github.com/pythonic-emacs/anaconda-mode
;; https://melpa.org/#/anaconda-mode
(use-package! anaconda-mode
  :bind (:map python-mode-map ("C-:" . company-anaconda))
  :hook
  (python-mode . anaconda-mode)
  (python-mode . anaconda-eldoc-mode)
  ;; :init
  ;; (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
)

;; IMPORTANT: check the benefits of lsp-jedi.
;; https://github.com/fredcamps/lsp-jedi
;; https://pypi.org/project/jedi-language-server/

;; ;; pip install -U jedi-language-server
;; (use-package lsp-jedi
;;   :ensure t
;;   :config
;;   (with-eval-after-load "lsp-mode"
;;     (message "HERE lsp-jedi")
;;     (add-to-list 'lsp-disabled-clients 'pyls)
;;     (add-to-list 'lsp-enabled-clients 'jedi)))

;;----------------------------------------------------------------------
;; Github Copilot.
;; https://github.com/zerolfx/copilot.el#example-for-doom-emacs

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (customize-set-variable 'copilot-enable-predicates nil)
  )

;;----------------------------------------------------------------------
;; codeium.el - Codeium is a code intelligence service that provides
;; completions, documentation, and more for many languages.
;; https://github.com/Exafunction/codeium.el

;; we recommend using use-package to organize your init.el
(use-package! codeium
    ;; if you use straight
    ;; :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-super-capf #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    :config
    (setq use-dialog-box nil) ;; do not use popup boxes

    ;; if you don't want to use customize to save the api-key
    ;; (setq codeium/metadata/api_key "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    (defun my-codeium/document/text ()
        (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; if you change the text, you should also change the cursor_offset
    ;; warning: this is measured by UTF-8 encoded bytes
    (defun my-codeium/document/cursor_offset ()
        (codeium-utf8-byte-length
            (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    (setq codeium/document/text 'my-codeium/document/text)
    (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;;----------------------------------------------------------------------
;; Add highlighting for certain keywords.

;; http://lists.gnu.org/archive/html/emacs-orgmode/2010-09/txtb5ChQJCDny.txt
;; http://emacs.1067599.n5.nabble.com/Adding-keywords-for-font-lock-experts-td95645.html

(dolist
    (mode '(fundamental-mode emacs-lisp-mode lisp-mode org-mode
            shell-mode sh-mode ess-mode ess-r-mode polymode-mode
            python-mode markdown-mode latex-mode TeX-mode
            prog-mode web-mode html-mode css-mode yaml-mode
            js-mode))
  (setq font-lock-keywords-case-fold-search t)
  (font-lock-add-keywords
   mode
   '(("\\(^\\|[[:space:]]\\)@[[:alnum:]_.]+\\>"
      0 'font-lock-function-name-face t))
   ;; @walmes, @param, @return
   ))

;;----------------------------------------------------------------------
;; hl-todo.

(defface hl-todo-caution-words
  '((t :foreground "OrangeRed"
       :background "LightGray"
       :inherit (hl-todo)))
  "Face for highlighting the CAUTION keyword.")

(defface hl-todo-good-words
  '((t :foreground "LightSeaGreen"
       :background "White"
       :inherit (hl-todo)))
  "Face for highlighting the GOOD/POSITIVE keyword.")

(defface hl-todo-bad-words
  '((t :foreground "White"
       :background "Firebrick"
       :inherit (hl-todo)))
  "Face for highlighting the BAD/NEGATIVE keyword.")

;; https://github.com/tarsius/hl-todo
(use-package! hl-todo
  :bind
  ("C-c l m" . hl-todo-previous)
  ("C-c l n" . hl-todo-next)
  :config
  ;; (message "final do arquivo")
  (global-hl-todo-mode t)
  (add-to-list 'hl-todo-keyword-faces '("IMPROVE"     font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("QUESTION"    font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("EXPLANATION" font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("THEORY"      font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("DESCRIPTION" font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("COMMENT"     font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("TIP"         font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("TRICK"       font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("INFO"        font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("INFORMATION" font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("DANGER"      error bold))
  (add-to-list 'hl-todo-keyword-faces '("STOP"        error bold))
  (add-to-list 'hl-todo-keyword-faces '("FAIL"        error bold))
  (add-to-list 'hl-todo-keyword-faces '("WARNING"     error bold))
  (add-to-list 'hl-todo-keyword-faces '("ERROR"       error bold))
  (add-to-list 'hl-todo-keyword-faces '("BUG"         error bold))
  (add-to-list 'hl-todo-keyword-faces '("DEBUG"       warning bold))
  (add-to-list 'hl-todo-keyword-faces '("IMPORTANT"   warning bold))
  (add-to-list 'hl-todo-keyword-faces '("ATTENTION"   warning bold))
  (add-to-list 'hl-todo-keyword-faces '("CAUTION"     warning bold))
  (add-to-list 'hl-todo-keyword-faces '("OBS"         warning bold))
  (add-to-list 'hl-todo-keyword-faces '("PROBLEM"     warning bold))
  (add-to-list 'hl-todo-keyword-faces '("DISCLAIMER"  warning bold))
  (add-to-list 'hl-todo-keyword-faces '("EXERCISE"    warning bold))
  (add-to-list 'hl-todo-keyword-faces '("BONUS"       success bold))
  (add-to-list 'hl-todo-keyword-faces '("DONE"        success bold))
  (add-to-list 'hl-todo-keyword-faces '("OKAY"        success bold))
  (add-to-list 'hl-todo-keyword-faces '("GOOD"        success bold))
  (add-to-list 'hl-todo-keyword-faces '("SOLVED"      success bold))
  (add-to-list 'hl-todo-keyword-faces '("OPTIONAL"    success bold))
  (add-to-list 'hl-todo-keyword-faces '("WALMES"    . hl-todo-good-words))
  )

;;----------------------------------------------------------------------
;; hl-prog-extra.
;; https://gitlab.com/ideasman42/emacs-hl-prog-extra

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
         '("\\<https?://[^[:blank:]]*" 1 comment success)
         ;; Match email address: <email@name.foo>.
         ;; '("<\\([[:alnum:]\\._-]+@[[:alnum:]\\._-]+\\)>" 1 comment success)
         )
        )
  :init
  (add-hook 'ess-mode-hook (lambda () (hl-prog-extra-mode))))

;;----------------------------------------------------------------------
