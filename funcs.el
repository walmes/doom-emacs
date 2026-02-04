;;----------------------------------------------------------------------
;; 1. UI & Navigation
;;----------------------------------------------------------------------

(defun wz-open-shell-split-window ()
  "Open shell in a split window."
  (interactive)
  (select-window (split-window-below))
  (shell))

(defvar wz-occur-saved-wconf nil
  "Saved window configuration for `wz-occur`.")

(defun wz-occur (&optional arg)
  "Make sure to always put occur in a vertical split, into a narrower
   buffer at the side."
  (interactive "P")
  ;; Store whatever frame configuration we are currently in.
  (setq wz-occur-saved-wconf (current-window-configuration))
  (occur (read-from-minibuffer "Regexp: "))
  (if (wz-occur-check-existence)
      (progn
        (delete-other-windows)
        (split-window-vertically)
        (enlarge-window -10)))
  (wz-occur-proceed-accordingly)
  (next-error-follow-minor-mode))

(defun wz-occur-proceed-accordingly ()
  "Switch to occur buffer or prevent opening of the occur window if no
   matches occurred."
  (interactive "P")
  (if (not (get-buffer "*Occur*"))
      (message "There are no results.")
    (switch-to-buffer "*Occur*")))

(defun wz-occur-check-existence ()
  "Signal the existence of an occur buffer."
  (interactive)
  (get-buffer "*Occur*"))

(defun wz-occur-mode-quit ()
  "Quit and close occur window, restoring previous window
   configuration."
  (interactive)
  (when wz-occur-saved-wconf
    (set-window-configuration wz-occur-saved-wconf)
    (setq wz-occur-saved-wconf nil)))

(defun wz-disable-fly-modes ()
  "Disable flymake and flycheck modes."
  (interactive)
  (when (bound-and-true-p flymake-mode)
    (flymake-mode -1))
  (when (bound-and-true-p flycheck-mode)
    (flycheck-mode -1))
  (message "Modes flymake and flycheck disabled."))


;;----------------------------------------------------------------------
;; 2. General Editing
;;----------------------------------------------------------------------

(defun wz-duplicate-line ()
  "Duplicate the current line without affecting the kill-ring."
  (interactive)
  (let ((line-text (buffer-substring (line-beginning-position)
                                     (line-end-position))))
    (save-excursion
      (move-end-of-line 1)
      (newline)
      (insert line-text))
    (forward-line 1)
    (message "Line duplicated.")))

(defun wz-copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (use-region-p)
      (progn
        (kill-ring-save (region-beginning) (region-end))
        (message "Copied region"))
    (kill-ring-save (line-beginning-position)
                    (line-beginning-position 2))
    (message "Copied line.")))

(defun wz-cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (use-region-p)
      (progn
        (kill-region (region-beginning) (region-end))
        (message "Cut region"))
    (kill-region (line-beginning-position)
                 (line-beginning-position 2))
    (message "Cut line.")))

(defun wz-comment-line-or-region ()
  "Comment or uncomment current line, or current text selection."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position)
                                 (line-beginning-position 2))))

(defun wz-mark-whole-word ()
  "Mark the symbol under the cursor (including . and _). The selection
   disappears automatically when the cursor moves."
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
        (when bounds
            (goto-char (cdr bounds))
            (push-mark (car bounds) t t)
            ;; Flash the selected region briefly.
            (pulse-momentary-highlight-region (car bounds) (cdr bounds)))))

(defun wz-what-face (pos)
  "Return the font face at point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun wz-unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line
   of text."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil)))

(defun wz-unfill-region (start end)
  "Replace newline chars in region by single spaces.
   This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun wz-split-name (s)
  "Split string `S` into a list of words based on camelCase or other
   delimiters."
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string
       "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun wz-camel-case (s)
  "Convert string `S` to camelCase."
  (concat (car (wz-split-name s))
          (mapconcat 'capitalize (cdr (wz-split-name s)) "")))

(defun wz-dot-case (s)
  "Convert string `S` to dot.case."
  (mapconcat 'downcase (wz-split-name s) "."))

(defun wz-snake-case (s)
  "Convert string `S` to snake_case."
  (mapconcat 'downcase (wz-split-name s) "_"))

(defun wz-camel-dot-snake ()
  "Cycle among camelCase, dot.case and snake_case in words. If the
   region is not active the current word at point is used."
  (interactive)
  (let ((is-region-active (and transient-mark-mode mark-active)))
    (unless is-region-active
      (let ((chars "[[:alnum:]]._"))
        (skip-chars-backward chars)
        (set-mark (point))
        (skip-chars-forward chars)))
    (let* ((beg (region-beginning))
           (end (region-end))
           (str (buffer-substring-no-properties beg end)))
      (if (string-match-p "^[[:space:]]*$" str)
          (message "Not a word at point")
        (delete-region beg end)
        (insert
         (cond ((string-match-p "\\." str) (wz-snake-case str))
               ((string-match-p "_"   str) (wz-camel-case str))
               (t                          (wz-dot-case   str))))
        (when is-region-active
          (setq deactivate-mark nil))))))

(defun wz-indent-and-move-to-next-line ()
  "Indent the current line and move cursor to the first printable
   character in the next line."
  (interactive)
  (indent-for-tab-command)
  (forward-line 1)
  (back-to-indentation))

(defun wz-comment-paragraph ()
  "Comment or uncomment the current paragraph. Return the cursor to its
   original position after execution."
  (interactive)
  (save-excursion
    (backward-paragraph)
    (mark-paragraph)
    (comment-dwim nil)))

(defun wz-indent-paragraph ()
  "Indent the current paragraph without _eating_ the leading blank
   line."
  (interactive)
  (save-excursion
    (let ((end (progn (forward-paragraph) (point)))
          (beg (progn (backward-paragraph)
                      ;; If not at the beginning of the file, skip the blank line.
                      (unless (bobp) (forward-line 1))
                      (point))))
      (indent-region beg end))))


;;----------------------------------------------------------------------
;; 3. Code Structure & Formatting
;;----------------------------------------------------------------------

(defun wz-blank-line-p ()
  "Return non-nil if current line is blank."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^[[:space:]]*$")))

(defun wz-insert-rule-from-point-to-margin (&optional char)
  "Insert a commented rule from `point' to `fill-column'. If the line is
   blank, it starts a comment. Useful to divide code into sections."
  (interactive)
  (let ((char (or char ?-)))
    (if (wz-blank-line-p)
        (progn
          (indent-according-to-mode)
          (insert comment-start)))
    (let ((count (max 0 (- fill-column (current-column)))))
      (insert (make-string count char)))
    (beginning-of-line)
    (forward-char fill-column)
    (delete-region (point) (line-end-position))))

(defun wz-insert-rule-and-comment-3 ()
  "Insert a commented rule with length relative to `fill-column'
   (62.5%)."
  (interactive)
  (if (wz-blank-line-p)
      (progn
        (indent-according-to-mode)
        (insert comment-start)))
  (let* ((column-middle (floor (* 0.625 fill-column)))
         (count (max 0 (- column-middle (current-column)))))
    (insert (make-string count ?-))))

(defun wz-left-align-commented-text (text)
  "Write TEXT aligned to the left and comment the line."
  (insert "\n" text)
  (comment-region (line-beginning-position) (point)))

(defun wz-right-align-commented-text (text comment-char-size)
  "Write TEXT aligned to the right margin at `fill-column' and comment
   it out."
  (insert "\n" text)
  (comment-region (line-beginning-position) (point))
  (backward-char (length text))
  (let ((spaces (- fill-column (length text) comment-char-size)))
    (when (> spaces 0)
      (insert (make-string spaces ?\s))))
  (forward-char (length text)))

(defun wz-header ()
  "Insert a header."
  (interactive)
  (wz-insert-rule-from-point-to-margin)
  ;; Get the number of characters used as comment.
  (let ((comment-char-size
         (- (+ fill-column 1)
            (how-many "-" (line-beginning-position) (point) t))))
    (wz-right-align-commented-text
     "Prof. Dr. Walmes M. Zeviani"
     comment-char-size)
    (wz-right-align-commented-text
     "leg.ufpr.br/~walmes · github.com/walmes"
     comment-char-size)
    (wz-right-align-commented-text
     "walmes@ufpr.br · @walmeszeviani"
     comment-char-size)
    (wz-right-align-commented-text
     "Laboratory of Statistics and Geoinformation (LEG)"
     comment-char-size)
    (wz-right-align-commented-text
     "Department of Statistics · Federal University of Paraná"
     comment-char-size)
    (wz-right-align-commented-text
     (concat (format-time-string "%Y-%b-%d") " · Curitiba/PR/Brazil")
     comment-char-size)
    )
  (insert "\n")
  (wz-insert-rule-from-point-to-margin))

;; Project header variables.
(defvar-local wz-project-title nil
    "Project title for the header.")
(defvar-local wz-project-subtitle nil
    "Project subtitle for the header.")
(defvar-local wz-project-author-name "Prof. Dr. Walmes M. Zeviani"
    "Author's name.")
(defvar-local wz-project-author-affiliation nil
    "Author's affiliation or work location.")
(defvar-local wz-project-author-email nil
    "Author's email or social networks.")
(defvar-local wz-project-author-website nil
    "Author's website or repository.")
(defvar-local wz-project-author-address nil
    "Author's address.")
(defvar-local wz-project-date nil
    "Project date.")

;; Define the local variables in the `.dir-locals.el' file following this
;; schema.
;;
;; ((nil . ((wz-project-title              . "Aplicações e Dashboards Avançado com R")
;;          (wz-project-subtitle           . "Usando Shiny & bslib")
;;          (wz-project-author-affiliation . "Universidade Federal da Grande Dourados")
;;          (wz-project-author-email       . "walmeszeviani@ufgd.edu.br")
;;          (wz-project-author-website     . "github.com/walmes · walmeszeviani.com")
;;          ;; (wz-project-author-address     . "Dourados, MS, Brasil")
;;          (wz-project-date               . "%Y-%b-%d · Dourados, MS, Brasil"))))

;; Function to insert a header for a project using local variables.
(defun wz-header-project ()
    "Insert a header for a project using local variables."
    (interactive)
    (let* ((char ?/)
           (comment-char-str (char-to-string char)))
      ;; Insert the initial rule with the / character.
      (wz-insert-rule-from-point-to-margin char)

      ;; Calculate the size of the comment prefix in the current buffer.
      ;; This is necessary for the right alignment function.
      (let ((comment-char-size
             (- (+ fill-column 2)
                (how-many comment-char-str
                          (line-beginning-position)
                          (point)
                          t))))

        ;; Title and subtitle left aligned.
        (when wz-project-title
          (wz-left-align-commented-text wz-project-title))
        (when wz-project-subtitle
          (wz-left-align-commented-text wz-project-subtitle))
        (wz-left-align-commented-text ".")
        (delete-char -2)
        ;; Author's information right aligned.
        (when wz-project-author-name
          (wz-right-align-commented-text
           wz-project-author-name
           comment-char-size))
        (when wz-project-author-affiliation
          (wz-right-align-commented-text
           wz-project-author-affiliation
           comment-char-size))
        (when wz-project-author-email
          (wz-right-align-commented-text
           wz-project-author-email
           comment-char-size))
        (when wz-project-author-website
          (wz-right-align-commented-text
           wz-project-author-website
           comment-char-size))
        (when wz-project-author-address
          (wz-right-align-commented-text
           wz-project-author-address
           comment-char-size))
        (when wz-project-date
          (let ((final-date (if (string-match-p "%" wz-project-date)
                                (format-time-string wz-project-date)
                              wz-project-date)))
            (wz-right-align-commented-text
             final-date comment-char-size)))
        )
      (insert "\n")
      (wz-insert-rule-from-point-to-margin char)))


;;----------------------------------------------------------------------
;; 4. R / ESS / Polymode
;;----------------------------------------------------------------------

;; --- Polymode / Rmd ---

(defun wz-polymode-insert-chunk ()
  "Insert R code chunk environment for Rmd/Qmd sessions. Positions
   cursor between opening and closing delimiters for editing."
  (interactive)
  (let ((in-ess-mode (derived-mode-p 'ess-mode)))
    (if in-ess-mode
        ;; ESS mode: chunk above current position
        (insert "```\n\n```{r}\n")
      ;; Other modes: chunk at current position
      (insert "```{r}\n\n```")))
  ;; Position cursor in the blank line inside the chunk
  (forward-line (if (derived-mode-p 'ess-mode) -2 -1))
  (end-of-line))

(defun wz-polymode-next-chunk ()
  "Move to the first line after the next code chunk header. Searches
   forward for the next chunk opening (```{...})."
  (interactive)
  (if (search-forward-regexp "^```{.*}$" nil t)
      (forward-line 1)
    (message "No more chunks found")))

(defun wz-polymode-previous-chunk ()
  "Move to the first line after the previous code chunk header. Searches
   backward for the previous chunk opening (```{...})."
  (interactive)
  (if (search-backward-regexp "^```$" nil t)
      (if (search-backward-regexp "^```{.*}$" nil t)
          (forward-line 1)
        (message "No previous chunks found"))
    (message "No previous chunks found")))

(defun wz-polymode-eval-R-chunk ()
  "Evals all code in R chunks in a polymode document (Rmd files)."
  (interactive)
  (if (derived-mode-p 'ess-mode)
      (let ((ptn (point))
            (beg (progn
                   (search-backward-regexp "^```{r.*}$" nil t)
                   (forward-line 1)
                   (line-beginning-position)))
            (end (progn
                   (search-forward-regexp "^```$" nil t)
                   (forward-line -1)
                   (line-end-position))))
        (ess-eval-region beg end nil)
        (goto-char ptn))
    (message "ess-mode weren't detected.")))

(defun wz-polymode-eval-R-chunk-and-next ()
  "Evals a R chunk and move point to next chunk."
  (interactive)
  (wz-polymode-eval-R-chunk)
  (wz-polymode-next-chunk))

(defun wz-polymode-eval-line-by-line (end)
  "This function evaluates only R code inside Rmd chunks from `point' to
   `end'. The function determine the boundaries of a chunk based on
   regex. If the point is a chunk, then each line is evaluated and the
   point go to the next line."
  (interactive)
  ;; Turn on/off the `inside-chunk' based on the major-mode at point.
  (if (derived-mode-p 'ess-mode)
      (setq inside-chunk t)
    (setq inside-chunk nil))
  ;; Go through each line till reach `end'.
  (while (< (point) end)
    ;; (beginning-of-line)
    ;; Test if point is at the chunk header.
    (if (looking-at "```{r")
        (progn
          (setq inside-chunk t)
          (forward-line 1)))
    ;; If inside-chunk is t then evaluate the line.
    (if inside-chunk
        (ess-eval-line t))
    (forward-line 1)
    ;; (beginning-of-line)
    ;; Test if point is at the chunk tail.
    (if (looking-at "```$")
        (setq inside-chunk nil))))

(defun wz-polymode-eval-chunks-on-region ()
  "This function calls `wz-polymode-eval-line-by-line'. It only
   determines if region is activated. Then it evaluates the code inside
   the chunks in the region."
  (interactive)
  (if (region-active-p)
      ;; If region is activated.
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char beg)
        ;; Calls the function.
        (wz-polymode-eval-line-by-line end))
    ;; If the region is not activated.
    (message "Region must be activated.")))

;; --- ESS / R Editing ---

(defun wz-ess-eval-word ()
  "Mark the symbol under the cursor and evaluate it in ESS/R. Unlike
   `mark-word`, this function respects dots and underscores common in R
   objects."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (save-excursion
          ;; 1. Define the region (beg and end)
          (let ((beg (car bounds))
                (end (cdr bounds)))
            ;; 2. Send to the ESS process.
            ;; Use `ess-send-region` because it is the most stable API.
            (ess-send-region (ess-get-process) beg end)))
      (message "No symbol (object) found under the cursor."))))

(defun wz-ess-forward-R-assigment-symbol ()
  "Move cursor to the next occurrence of `<-' or single `='. Excludes
   `==' and `!='."
  (interactive)
  (search-forward-regexp "<-\\|[^!=]=[^=]" nil t))

(defun wz-ess-backward-R-assigment-symbol ()
  "Move cursor to the previous occurrence of `<-' or single `='.
   Excludes `==' and `!='."
  (interactive)
  (search-backward-regexp "<-\\|[^!=]=[^=]" nil t))

(defun wz-ess-align-R-assigment-operators ()
  "Align assignment operators (`<-' or `=') in the active region."
  (interactive)
  (save-excursion
    (align-regexp
     (region-beginning) (region-end)
     "\\(\\s-*\\) *\\(<-\\|[^!=]=[^=]\\) *" 1 1 nil)))

(defun wz-ess-backward-break-line-here ()
  "Searches a point backward where a break there is allowed."
  (interactive)
  (re-search-backward "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (forward-char 1))

(defun wz-ess-forward-break-line-here ()
  "Searches a point forward where a break there is allowed. I
   don't know why, but the forward some times skips correct
   points that backward get."
  (interactive)
  (re-search-forward "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (forward-char -1))

(defun string-face-p ()
  "Return t if font-face at point is string-face, nil otherwise."
  (eq 'font-lock-string-face (get-char-property (point) 'face)))

(defun comment-face-p ()
  "Return t if font-face at point is comment-face, nil otherwise."
  (eq 'font-lock-comment-face (get-char-property (point) 'face)))

(defun wz-ess-break-or-join-lines-wizard ()
  "Break line wizard in R scripts. This function helps break and indent
   or join lines in R code. The keybings are:
   <right>    : go to next matching;
   <left>     : go to previous matching;
   <down>     : go to next line;
   <up>       : go to previous line;
   <return>   : break and indent newline;
   <delete>   : join line below;
   <backspace>: join line above;
   <C-z>      : undo;
   <any>      : unhighlight and exit."
  (interactive)
  (setq rgxp "\\([-+*/%<>(,]\\|[<>=!]=\\) *[[:alnum:]({]")
  (highlight-regexp rgxp)
  (let (done event)
    (while (not done)
      (let ((inhibit-quit t))
        (setq event (read-event
                     "Press: right|left|down|up|RET|DEL|BKS|any"))
        (if inhibit-quit (setq quit-flag nil))
        (cond ((eq event 'right)
               (progn (re-search-forward rgxp)
                      (while (or (string-face-p) (comment-face-p))
                        (re-search-forward rgxp))
                      (forward-char -1)))
              ((eq event 'left)
               (progn (re-search-backward rgxp)
                      (while (or (string-face-p) (comment-face-p))
                        (re-search-backward rgxp))
                      (forward-char 1)
                      ))
              ((eq event 'return)
               (progn (indent-new-comment-line)
                      (re-search-forward rgxp)
                      (forward-char -1)))
              ((eq event 'delete)
               (progn (forward-line)
                      (delete-indentation)
                      (re-search-backward rgxp)
                      (forward-char 1)))
              ((eq event 'backspace)
               (progn (delete-indentation)
                      (re-search-backward rgxp)
                      (forward-char 1)))
              ((eq event 'down)
               (forward-line))
              ((eq event 'up)
               (forward-line -1))
              ((eq event ?\C-z)
               (undo))
              ((eq event 'escape)
               (setq done t))
              (t
               (setq done t)))))
    (unhighlight-regexp rgxp)))

(defun wz-align-by-separator (beg end separator)
  "Align the selected region using SEPARATOR. Ensures all lines (except
   the last) end with the separator and preserves the original block
   indentation.
   Before:
     ~mpg, ~cyl, ~disp,
     21,6,160,
     21,6,160
   After:
     ~mpg, ~cyl, ~disp,
       21,    6,   160,
       21,    6,   160"
  (interactive "r\nsSeparator (e.g., ,): ")
  (let ((sep (if (string-equal separator "") "," separator))
        ;; Capture the indentation of the first line to replicate in the block.
        (indent (save-excursion
                  (goto-char beg)
                  (current-indentation))))
    (save-excursion
      (save-restriction
        ;; Narrow the region to avoid side effects in the file.
        (narrow-to-region beg end)

        ;; 1. Preparation: transform separators into temporary pipes.
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote sep) nil t)
          (replace-match " | "))

        ;; 2. Normalization: create the Org table structure.
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (delete-horizontal-space)
          (insert "| ")
          (end-of-line)
          ;; Ensure each line ends with a pipe for Org.
          (unless (looking-back "| " 2) (insert " |"))
          (forward-line 1))

        ;; 3. Alignment: Org's engine does the heavy lifting.
        (require 'org-table)
        (org-table-align)

        ;; 4. Reversion: remove pipes and apply the separator logic.
        (goto-char (point-min))
        (let ((last-line-pos (save-excursion
                               (goto-char (point-max))
                               (forward-line 0)
                               (point))))
          (while (not (eobp))
            (let ((is-last (= (line-beginning-position) last-line-pos)))
              ;; Remove the opening pipe of the table.
              (beginning-of-line)
              (when (looking-at "| ") (delete-char 2))

              ;; Handle the closing pipe (the secret for the final comma).
              (end-of-line)
              (when (re-search-backward " |$" (line-beginning-position) t)
                (if is-last
                    (replace-match "")      ; Last line: clean.
                  (replace-match sep)))    ; Others: add the separator.

              ;; Convert internal pipes back to the separator.
              (goto-char (line-beginning-position))
              (while (re-search-forward " | " (line-end-position) t)
                (replace-match (concat sep " ")))

              ;; Restore the original indentation captured at the start.
              (beginning-of-line)
              (indent-to indent)
              (forward-line 1))))

        ;; Cleanup of unnecessary trailing spaces.
        (delete-trailing-whitespace)
        (goto-char (point-max))
        (delete-char -2)))))

(defun wz-bm-bookmark-chunk-in-buffer ()
  "Set bookmark on the first line inside each chunk in Rnw and Rmd
   files. Bookmarks are placed just after the chunk header, inside the
   code block."
  (interactive)
  (let ((regexp "^<<.*>>=$\\|^```{.*}$")
        (annotation nil)
        (count 0))
    (save-excursion
      (if bm-annotate-on-create
          (setq annotation
                (read-from-minibuffer
                 "Annotation: " nil nil nil 'bm-annotation-history)))
      (goto-char (point-min))
      (while (and (< (point) (point-max))
                  (re-search-forward regexp (point-max) t))
        (forward-line 1) ; move to the first line inside the chunk
        (bm-bookmark-add annotation)
        (setq count (1+ count))
        (forward-line 1)))
    (message "%d bookmark(s) created." count)))

;; https://github.com/basille/.emacs.d/blob/master/functions/ess-indent-region-as-R-function.el
;; The function below is a modification of the original to use
;; formatR::tidy_source(). This is because formatR have functions with
;; arguments to control the output, as keep comments and set the width
;; cutoff.

(defun wz-ess--region-to-escaped-string (beg end)
  "Return escaped string from region between BEG and END. Escapes quotes
   to safely embed the text in an R string literal."
  (replace-regexp-in-string
   "\"" "\\\\\\&"
   (replace-regexp-in-string
    "\\\\\"" "\\\\\\&"
    (buffer-substring-no-properties beg end))))

(defun wz-ess--replace-region-with-ess-output (beg end buf &optional skip-style trim-chars)
  "Replace region between BEG and END with the output in BUF. SKIP-STYLE
   controls how to skip the header in BUF: use 'eol to skip the first
   line, or anything else to skip leading spaces. TRIM-CHARS removes
   characters from the end of the inserted string."
  (let (string)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((output-end (point)))
        (goto-char (point-min))
        (pcase skip-style
          ('eol (goto-char (1+ (point-at-eol))))
          (_ (skip-chars-forward " +")))
        (setq string (buffer-substring-no-properties (point) output-end))))
    (delete-region beg end)
    (insert string)
    (when (and trim-chars (> trim-chars 0))
      (delete-char (- trim-chars)))))

(defun wz-ess-indent-region-with-formatR-tidy-source (beg end)
  "Format region of code R using `formatR::tidy_source()'."
  (interactive "r")
  (let ((string (wz-ess--region-to-escaped-string beg end))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          formatR::tidy_source(text = \"\n%s\",
                                arrow = TRUE, width.cutoff = 60) })\n"
      string) buf)
    (wz-ess--replace-region-with-ess-output beg end buf 'eol)))

(defun wz-ess-stringi-escape-unicode (beg end)
  "Replace non-ASCII by the corresponding unicode. Select the text
   without the quotes and apply the function."
  (interactive "r")
  (let ((string (wz-ess--region-to-escaped-string beg end))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          cat(stringi::stri_escape_unicode(\"%s\"),
              \"\\n\") })\n"
      string) buf)
    (wz-ess--replace-region-with-ess-output beg end buf nil 2)))

(defun wz-ess-find-and-insert-namespace (beg end)
  "Preceds a function with its namespace, so `mean(x) -> stats::mean(x)'
   and `xyplot(...) -> lattice::xyplot()'. Call this function in a R
   major mode buffer with the function name selected."
  (interactive "r")
  (let ((string (wz-ess--region-to-escaped-string beg end))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
           x <- \"%s\"
           cat(paste0(sub('.*:', '', utils::find(x)), '::', x), \"\\n\")
       })\n"
      string) buf)
    (wz-ess--replace-region-with-ess-output beg end buf nil 1)))

(defun wz-ess-insert-function-args (beg end)
  "This function inserts all arguments of a function call. When you mark
   the word `plot' this function returns `plot(x, y, ....)'."
  (interactive "r")
  (let ((string (wz-ess--region-to-escaped-string beg end))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
           usage <- function(f = 'lm', shift = 0L) {
               f_width <- nchar(f)
               u <- capture.output(args(f)) |>
                   sub(pattern = 'function [(]',
                       replacement = paste0(f, '(')) |>
                   head(n = -1)
               u <- u |>
                   paste(collapse = '') |>
                   gsub(pattern = '[[:space:]]+', replacement = ' ') |>
                   strsplit(split = ', ')
               u <- u[[1]]
               u[-length(u)] <- paste0(u[-length(u)], ',\n')
               spaces <- c(paste(rep(' ', shift),
                                 collapse = ''),
                           paste(rep(' ', shift + f_width - 1),
                                 collapse = ''))
               u[1] <- paste(spaces[1], u[1])
               u[-1] <- paste(spaces[-1], u[-1])
               cat(u, '\n')
           }
           usage(\"%s\", %d)
       })\n"
      string (save-excursion (goto-char beg) (current-column))) buf)
    (wz-ess--replace-region-with-ess-output beg end buf nil 1)
    (goto-char end)))

(defun wz-ess-open-html-documentation (beg end)
  "Open HTML documentation for the object at point in R."
  (interactive "r")
  (let ((string (wz-ess--region-to-escaped-string beg end))
        (buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
           help(%s, help_type = \"html\")
       })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      (let ((end (point)))
        (goto-char (point-min))
        (skip-chars-forward " +")
        (setq string (buffer-substring-no-properties (point) end))))
    )
  )

(defun wz-ess-newline-indented ()
  "Execute `ess-roxy-newline' and `ess-indent-or-complete'."
  (interactive)
  (if (nth 2 (syntax-ppss))
      (progn
        (comment-normalize-vars)
        (comment-indent-new-line))
    (ess-roxy-newline)
    (ess-indent-or-complete)))

(defun wz-ess-one-argument-by-line-and-indent-region (start end)
  "Break lines after commas, open parentheses, and closing
   parentheses in the selected region, excluding strings."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[,()]" end t)
      (let ((in-string (nth 3 (syntax-ppss)))
            (before-operator
             (or (eq (char-after (match-end 0)) ?\n)
                 (save-excursion
                   (goto-char (match-end 0))
                   (looking-at " *[+*/%^),]")))
             )
            )
        (unless (or in-string before-operator)
          (replace-match (concat (match-string 0) "\n")))
        ) ;; let
      ) ;; while
    (indent-region start end)
    ;; (ess-indent-or-complete)
    ) ;; save-excursion
  )

(defun wz-ess-cancel-on-inferior-ess-buffer ()
  "Switch to the inferior ESS buffer, interrupt any running job, and
   switch back to the script buffer."
  (interactive)
  (ess-switch-to-inferior-or-script-buffer t)
  (comint-interrupt-subjob)
  (ess-switch-to-inferior-or-script-buffer t))

(defun wz-ess-roxy-toggle-custom (beg end)
  "Toggle the roxygen prefix (#' or ##') robustly. Handles blank lines
   correctly and prioritizes the #' style."
  (interactive "r")
  (let ((beg (if (use-region-p) beg (line-beginning-position)))
        (end (if (use-region-p) end (line-end-position))))
    (save-excursion
      (save-restriction
        ;; Narrow the region to avoid side effects.
        (narrow-to-region beg end)
        (goto-char (point-min))
        ;; Decide the action based on the first line of the region.
        (let ((is-roxygen (looking-at "^#+'")))
          (while (not (eobp))
            (beginning-of-line)
            (if is-roxygen
                ;; REMOVE: Match #' or ##' followed by zero or one space.
                (when (looking-at "^#+' ?")
                  (replace-match ""))
              ;; ADD: Insert the default roxygen prefix.
              (unless (looking-at "^$") ; Avoid extra space on blank lines.
                (insert "#' "))
              (when (looking-at "^$")
                (insert "#'")))
            (forward-line 1)))))))


;;----------------------------------------------------------------------
;; 5. LSP / Treemacs / Imenu
;;----------------------------------------------------------------------

;; Already defined in LSP: "<f8>"
(defun lsp-ui-imenu-toggle ()
  "Toggle the lsp-ui-imenu buffer."
  (interactive)
  (if (get-buffer "*lsp-ui-imenu*")
      (kill-buffer "*lsp-ui-imenu*")
    (lsp-ui-imenu)
    )
  )

;; Already defined in LSP: "C-<f8>"
(defun lsp-treemacs-symbols-toggle ()
  "Toggle the lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

;;----------------------------------------------------------------------
;; 6. Themes
;;----------------------------------------------------------------------

(defvar wz-theme-list '(doom-one
                        doom-one-light
                        doom-dracula
                        doom-nord-light)
  "List of themes to cycle through. Add or remove theme names here.")

(defun wz-cycle-themes ()
  "Cycle through themes defined in `wz-theme-list`."
  (interactive)
  (let* ((current (car custom-enabled-themes))
         (next (or (cadr (member current wz-theme-list))
                   (car wz-theme-list))))
    ;; 1. Disable all active themes to avoid color "bleeding"
    (mapc #'disable-theme custom-enabled-themes)
    ;; 2. Load the next theme (the 't' bypasses the safety prompt)
    (load-theme next t)
    (message "Current theme: %s" next)))


;;----------------------------------------------------------------------
;; 7. Keybindings
;;----------------------------------------------------------------------

;; ---- Global shortcuts (available in all modes) ----------------------

(map! "M-Q"         #'wz-unfill-region
      "C-S-o"       #'wz-occur
      "S-<delete>"  #'wz-cut-line-or-region
      "C-<insert>"  #'wz-copy-line-or-region
      "C-<f12>"     #'wz-cycle-themes
      "C-c d"       #'wz-duplicate-line
      "C-x w"       #'wz-mark-whole-word
      "C-x t"       #'wz-open-shell-split-window
      "M-;"         #'wz-comment-line-or-region
      "C-ç"         #'wz-camel-dot-snake
      "<C-i>"       #'wz-indent-and-move-to-next-line
      "M-."         #'wz-comment-paragraph
      "M-+"         #'wz-indent-paragraph

      ;; Insert rules and separators
      "C-c -"       #'wz-insert-rule-from-point-to-margin
      "C-M--"       #'wz-insert-rule-and-comment-3

      ;; Use cmd! to replace repetitive lambdas
      "C-c ="       (cmd! (wz-insert-rule-from-point-to-margin ?=))
      "C-c /"       (cmd! (wz-insert-rule-from-point-to-margin ?/))
      "C-c ,"       (cmd! (wz-insert-rule-from-point-to-margin ?¬))
      "C-c ."       (cmd! (wz-insert-rule-from-point-to-margin ?─)))

(after! replace
    ;; Occur Mode typically resides in the 'replace' package.
    (map! :map occur-mode-map
          "q" #'wz-occur-mode-quit))

;; ---- Shared shortcuts (Markdown & ESS) ------------------------------

(after! markdown-mode
  (map! :map (markdown-mode-map ess-mode-map)
        "C-c i i" #'wz-polymode-insert-chunk
        "<f6>"    #'wz-polymode-eval-R-chunk
        "S-<f6>"  #'wz-polymode-eval-R-chunk-and-next
        "S-<f7>"  #'wz-polymode-previous-chunk
        "S-<f8>"  #'wz-polymode-next-chunk))

;; ---- ESS-specific shortcuts (R) -------------------------------------

(after! ess-roxy
  (map! :map ess-roxy-mode-map
        "C-c C-o C-c" #'wz-ess-roxy-toggle-custom))

(after! ess-mode
  (map! :map ess-mode-map
        "C-<escape>"  #'wz-ess-cancel-on-inferior-ess-buffer
        "<C-f1>"      #'wz-ess-open-html-documentation
        "<C-f4>"      #'wz-ess-insert-function-args
        "<f7>"        #'wz-ess-break-or-join-lines-wizard
        "C-c r"       #'wz-ess-eval-word
        "C-c a"       #'wz-ess-align-R-assigment-operators
        "C-,"         #'wz-ess-backward-break-line-here
        "C-."         #'wz-ess-forward-break-line-here
        "C-:"         #'wz-ess-find-and-insert-namespace
        "S-<f9>"      #'wz-ess-backward-R-assigment-symbol
        "S-<f10>"     #'wz-ess-forward-R-assigment-symbol
        "M-j"         #'wz-ess-newline-indented
        "S-<return>"  #'wz-ess-newline-indented
        "<backtab>"   #'wz-ess-one-argument-by-line-and-indent-region
        "C-M-|"       #'wz-ess-indent-region-with-formatR-tidy-source
        "C-?"         #'wz-ess-stringi-escape-unicode))

;;----------------------------------------------------------------------

(provide 'funcs)
