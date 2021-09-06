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
;; (setq doom-font
;;       (font-spec :family "monospace"
;;                  :size 12
;;                  :weight 'semi-light)
;;       doom-variable-pitch-font
;;       (font-spec :family "sans"
;;                  :size 13))

;; There are two ways to load a theme. Both assume the theme is
;; installed and available. You can either set `doom-theme' or manually
;; load a theme with the `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
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
(global-whitespace-mode +1)
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
;; Web-mode.

(use-package! treemacs
  :config
  (setq treemacs-is-never-other-window nil))

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
  :config
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
   ("C-S-<f9>" . ess-noweb-goto-chunk))
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
      (ess-toggle-underscore nil)
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
;; Python as a IDE with REPL.

;; Requirements on:
;; https://github.com/hlissner/doom-emacs/tree/develop/modules/lang/python
;;   cd anaconda
;;   source activate
;;   pip install pytest nose black pyflakes isort
;;   pip install python-language-server[all]
;;   pip install pyright
;;   conda deactivate
;; On Emacs:
;;   M-x lsp-install-server RET mspyls
;;   M-x lsp-python-ms-setup RET
;; https://github.com/microsoft/pyright

;; ATTENTION:
;; Choose an anaconda Env:
;;   M-x conda-env-activate base
;; Restart LSP:
;;   M-x lsp ...or... M-x lsp-restart-workspace

(use-package! elpy
  :init
  (elpy-enable)
  :config
  (progn
    (setq python-indent-offset 4)
    (define-key python-mode-map [f5] 'company-complete)
    (define-key python-mode-map [f6] 'complete-symbol)
    ;; Elpy will install RPC dependencies automatically.
    (setq elpy-rpc-python-command "/home/walmes/anaconda/bin/python3")
    (setq python-shell-interpreter "/home/walmes/anaconda/bin/python3")
    ))

;; TODO FIXME: needs to understand this Env thing.
;; /home/walmes/anaconda/bin/python3

;; https://enzuru.medium.com/helpful-emacs-python-mode-hooks-especially-for-type-hinting-c4b70b9b2216
(add-hook
 'python-mode-hook
 (lambda ()
   (anaconda-mode)
   (anaconda-eldoc-mode)
   (flycheck-mode nil)
   (setq lsp-diagnostics-provider :none)
   ;; This configures `pyls' language server.
   (setq lsp-clients-python-command "/home/walmes/anaconda/bin/pyls")
   (setq lsp-pyls-plugins-pylint-enabled t)
   (setq lsp-pyls-plugins-autopep8-enabled nil)
   (setq lsp-pyls-plugins-yapf-enabled t)
   (setq lsp-pyls-plugins-pyflakes-enabled nil)
   ;; (local-set-key (kbd "C-x C-d") 'anaconda-mode-show-doc)
   ;; (local-set-key (kbd "C-x C-w") 'anaconda-mode-find-definitions)
   ))

;; (use-package! lsp-python-ms
;;   :config
;;   ;; these hooks can't go in the :hook section since lsp-restart-workspace
;;   ;; is not available if lsp isn't active
;;   ;; (setq lsp-python-ms-extra-paths [ "/home/walmes/anaconda/bin/python3" ])
;;   (add-hook 'conda-postactivate-hook
;;             (lambda () (lsp-restart-workspace)))
;;   (add-hook 'conda-postdeactivate-hook
;;             (lambda () (lsp-restart-workspace))))

;; https://www.reddit.com/r/emacs/comments/hkshob/save_correct_condaenv_for_project/fwxty9v?utm_source=share&utm_medium=web2x&context=3
;;
;; 1. Set `conda-project-env-name' as a directory local variable. (With
;;    projectile you could use the `projectile-edit-dir-locals'
;;    command.)
;;
;; 2. Use `conda-env-activate-for-buffer' to activate the environment
;;    set. (Or enable `conda-env-autoactivate-mode' to automatically
;;    activate it.)

(use-package! conda
  :init
  (setq conda-anaconda-home (expand-file-name "~/anaconda"))
  (setq conda-env-home-directory (expand-file-name "~/anaconda"))
  :config
  (conda-env-initialize-interactive-shells)
  (conda-env-initialize-eshell))

(use-package! anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

;; IMPORTANT: check the benefits of lsp-jedi.
;; https://github.com/fredcamps/lsp-jedi
;; https://pypi.org/project/jedi-language-server/

;;----------------------------------------------------------------------
;; Latex extensions.

;; Disable `latex' module because it brings a bug related to
;; fontification. Since I'm not using LaTeX frequetly these days,
;; enabling just AUCTex is enougth.
;; https://www.reddit.com/r/emacs/comments/g95vyl/font_lock_in_auctex_messed_up/

(use-package! auctex
  :mode
  (("\\.pgf\\'" . latex-mode)
   ("\\.pgs\\'" . latex-mode))
  :config
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

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
  (message "final do arquivo")
  (global-hl-todo-mode t)
  (add-to-list 'hl-todo-keyword-faces '("IMPROVE"     font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("QUESTION"    font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("EXPLANATION" font-lock-constant-face bold))
  (add-to-list 'hl-todo-keyword-faces '("DESCRIPTION" font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("COMMENT"     font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("TIP"         font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("TRICK"       font-lock-keyword-face bold))
  (add-to-list 'hl-todo-keyword-faces '("DANGER"      error bold))
  (add-to-list 'hl-todo-keyword-faces '("STOP"        error bold))
  (add-to-list 'hl-todo-keyword-faces '("FAIL"        error bold))
  (add-to-list 'hl-todo-keyword-faces '("IMPORTANT"   warning bold))
  (add-to-list 'hl-todo-keyword-faces '("ATTENTION"   warning bold))
  (add-to-list 'hl-todo-keyword-faces '("OBS"         warning bold))
  (add-to-list 'hl-todo-keyword-faces '("PROBLEM"     warning bold))
  (add-to-list 'hl-todo-keyword-faces '("DONE"        success bold))
  (add-to-list 'hl-todo-keyword-faces '("OKAY"        success bold))
  (add-to-list 'hl-todo-keyword-faces '("GOOD"        success bold))
  (add-to-list 'hl-todo-keyword-faces '("SOLVED"      success bold))
  (add-to-list 'hl-todo-keyword-faces '("WALMES"    . hl-todo-good-words))
  )

;;----------------------------------------------------------------------
