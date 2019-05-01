(defun init/packages ()
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa" . "http://melpa.org/packages/")))

  (require 'package)
  (package-initialize)
  ;;(package-refresh-contents)

  (setq my-packages
        '(
          ;; themes
          zenburn-theme
          color-theme-sanityinc-tomorrow
          plan9-theme
          atom-one-dark-theme
          planet-theme
          nord-theme

          ;; syntax
          clojure-mode
          markdown-mode
          yaml-mode
          dockerfile-mode

          company
          cider
          paredit
          smartparens
          rainbow-mode
          magit
          zoom-window
          helm
          helm-themes
          helm-ag
          helm-projectile
          neotree
          projectile
          undo-tree
          rainbow-delimiters
          highlight-parentheses
          multiple-cursors
          clj-refactor
          swiper
          counsel
          expand-region
          counsel-projectile
          github-browse-file
          s
          hydra
          which-key
          perspective
          exec-path-from-shell
          ))

  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun select-region (begin end)
  (goto-char begin)
  (set-mark-command nil)
  (goto-char end))

(defun select-sexp-at-point (&optional arg dont-kill)
  (interactive "P")
  (require 'smartparens)
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point)))
    (cond
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp-compare-sexps next enc)
            (when (not dont-kill)
              (let ((del (sp-get-whitespace)))
                (sp-get del (delete-region :beg :end))))
          (if (> arg 0)
              (sp--kill-or-copy-region
               (sp-get next :beg-prf) (sp-get enc :end-in) dont-kill)
            (sp--kill-or-copy-region
             (sp-get next :end) (sp-get enc :beg-in) dont-kill))
          (when (not dont-kill)
            (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end)))))))
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (sp--kill-or-copy-region
                     :beg-prf :end dont-kill))))
     ((= n 0)
      (let ((e (sp-get-enclosing-sexp)))
        (when e
          (sp-get e (sp--kill-or-copy-region
                     :beg-in :end-in dont-kill)))))
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (sp-get ok
            (when (< :beg-prf b) (setq b :beg-prf))
            (when (> :end e) (setq e :end)))
          (setq n (1- n))))
      (when ok
        (let ((bm (set-marker (make-marker) b)))
          (if (eq last-command 'kill-region)
              (progn
                (when (member sp-successive-kill-preserve-whitespace '(1 2))
                  (kill-append sp-last-kill-whitespace nil))
                (select-region (if (> b (point)) (point) b) e))
            (select-region b e))
          (when (not dont-kill)
            (sp--cleanup-after-kill)
            (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
              (setq sp-last-kill-whitespace
                    (concat sp-last-kill-whitespace
                            (buffer-substring-no-properties bm (point))))
              (select-region bm (point)))
            (when (= 0 sp-successive-kill-preserve-whitespace)
              (kill-append sp-last-kill-whitespace nil)))))))))

;; got from https://github.com/noprompt/matilde

(defun char-at-point ()
  (interactive)
  (buffer-substring-no-properties (point) (+ 1 (point))))

(defun clj/string-name (s)
  (substring s 1 -1))

(defun clj/keyword-name (s)
  (substring s 1))

(defun clj/delete-and-extract-sexp ()
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun clj/toggle-keyword-string ()
  (interactive)
  (save-excursion
    (if (equal 1 (point))
        nil
      (cond
       ((equal "\"" (char-at-point))
        (insert ":" (clj/string-name
                     (clj/delete-and-extract-sexp))))
       ((equal ":" (char-at-point))
        (insert "\"" (clj/keyword-name
                      (clj/delete-and-extract-sexp)) "\""))
       (t (progn
            (backward-char)
            (clj/toggle-keyword-string)))))))

(defun clj/ignore ()
  (interactive)
  (when (not (string-equal (string (following-char)) "(")) ;; TODO {} []
    (paredit-backward-up))
  (insert "#_"))

(defun search-symbol-at-point ()
  (interactive)
  ;; FIXME - select-sexp-at-point kills custom indentation
  (select-sexp-at-point)
  (helm-projectile-ag))

(defun re-frame-jump-to-reg () ;; https://github.com/oliyh/re-jump.el
  (interactive)

  (require 'cider-util)
  (require 'cider-resolve)
  (require 'cider-client)
  (require 'cider-common)
  (require 'cider-interaction)
  (require 'clojure-mode)
  (let* ((kw (cider-symbol-at-point 'look-back))
         (ns-qualifier (and
                        (string-match "^:+\\(.+\\)/.+$" kw)
                        (match-string 1 kw)))
         (kw-ns (if ns-qualifier
                    (cider-resolve-alias (cider-current-ns) ns-qualifier)
                  (cider-current-ns)))
         (kw-to-find (concat "::" (replace-regexp-in-string "^:+\\(.+/\\)?" "" kw))))

    (when (and ns-qualifier (string= kw-ns (cider-current-ns)))
      (error "Could not resolve alias \"%s\" in %s" ns-qualifier (cider-current-ns)))

    (progn (cider-find-ns "-" kw-ns)
           (search-forward-regexp (concat "[a-zA-Z-]*[ \\\n]+" kw-to-find) nil 'noerror)
           ;;(search-forward-regexp kw-to-find nil 'noerror)
           )))

(defun jump-to-current-version ()
  (interactive)
  (when (fboundp 'projectile-project-root)
    (when-let ((name (buffer-name)))
      (let ((cpoint (point))
            (filename (concat (projectile-project-root)
                              (car (split-string name ".~")))))
        (if (file-exists-p filename)
            (progn
              (find-file filename)
              (goto-char cpoint))
          (message "File doesn't exist: %s" filename))))))

(defun sp-clone-sexp-noindent ()
  (interactive)
  (require 'smartparens)
  (-when-let (ok (let ((sexp (sp-get-thing)))
                   (if (not (equal (sp-get sexp :op) ""))
                       sexp
                     (sp-get-enclosing-sexp))))
    (sp-get ok
      (undo-boundary)
      (if (< :beg-prf (point))
          (save-excursion
            (goto-char :beg-prf)
            (insert-buffer-substring-no-properties
             (current-buffer) :beg-prf :end-suf)
            (newline-and-indent))
        (goto-char :end-suf)
        (save-excursion
          (insert-buffer-substring-no-properties
           (current-buffer) :beg-prf :end-suf))
        (newline-and-indent)))))

(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))
          (insert text))))
    (if use-region nil
      (let ((pos (- (point) (line-beginning-position))))
        (if (> 0 n)
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun delete-space-forward ()
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-forward " \t\n") (point))))

(defun delete-space-backward ()
  "*Delete all spaces and tabs after point."
  (interactive "*")
  (delete-region (point) (progn (skip-chars-backward " \t\n") (point))))

;; https://emacs.stackexchange.com/questions/29664/how-to-do-paredit-kill-backwards
(defun paredit-backward-delete-line ()
  "Delete line backwards, preserving delimiters and not adding to the kill ring."
  (require 'paredit)
  (interactive)
  (setq-local paredit--started-in-string-p (paredit-in-string-p))
  (setq-local paredit--backward-region-p nil)
  (if (or (and (not (paredit-in-char-p (1- (point))))
               (not (paredit-in-comment-p))
               (eq (char-syntax (char-before)) ?\) ))
          (and (not (paredit-in-string-p))
               (eq (char-syntax (char-before)) ?\" )))
      (progn
        (set-mark-command nil)
        (setq deactivate-mark nil)
        (paredit-backward)
        (setq-local paredit--backward-region (buffer-substring (region-beginning) (region-end)))
        (setq-local paredit--backward-region-p t)
        (delete-active-region)))
  (if (or (null paredit--backward-region-p) (<= (s-count-matches "\n" paredit--backward-region) 1))
      (dotimes (i (current-column))
        (if (and (not (paredit-in-char-p (1- (point))))
                 (not (paredit-in-comment-p))
                 (eq (char-syntax (char-before)) ?\) ))
            (paredit-backward-delete-line))
        (unless (or (and (not (paredit-in-char-p (1- (point))))
                         (not (paredit-in-comment-p))
                         (eq (char-syntax (char-before)) ?\( )
                         (eq (char-after) (matching-paren (char-before))))
                    (and paredit--started-in-string-p
                         (eq (1- (point)) (car (paredit-string-start+end-points)))
                         (eq (point) (cdr (paredit-string-start+end-points)))))
          (if (paredit-in-comment-p) (delete-backward-char 1) (paredit-backward-delete 1)))))
  (message nil))

(defun sp-delete-sexp (&optional arg)
  (interactive "P")
  (require 'smartparens)
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point)))
    (cond
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp-compare-sexps next enc)
              (let ((del (sp-get-whitespace)))
                (sp-get del (delete-region :beg :end)))
          (if (> arg 0)
              (delete-region
               (sp-get next :beg-prf) (sp-get enc :end-in))
            (delete-region
             (sp-get next :end) (sp-get enc :beg-in)))
          (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end))))))
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (sp-get lst (delete-region
                     :beg-prf :end))))
     ((= n 0)
      (let ((e (sp-get-enclosing-sexp)))
        (when e
          (sp-get e (delete-region
                     :beg-in :end-in)))))
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (sp-get ok
            (when (< :beg-prf b) (setq b :beg-prf))
            (when (> :end e) (setq e :end)))
          (setq n (1- n))))
      (when ok
        (let ((bm (set-marker (make-marker) b)))
          (if (eq last-command 'kill-region)
              (progn
                (when (member sp-successive-kill-preserve-whitespace '(1 2))
                  (kill-append sp-last-kill-whitespace nil))
                (delete-region
                 (if (> b (point)) (point) b) e))
            (delete-region b e))
          (sp--cleanup-after-kill)
          (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
            (setq sp-last-kill-whitespace
                  (concat sp-last-kill-whitespace
                          (buffer-substring-no-properties bm (point))))
            (delete-region bm (point)))
          (when (= 0 sp-successive-kill-preserve-whitespace)
            (kill-append sp-last-kill-whitespace nil))))))))

(defun paste-sexp-with-replace ()
  "Deletes selected sexp and pastes previously copied one"
  (interactive)
  (sp-delete-sexp)
  (yank))

(defun copy-surrounding-sexp ()
  (interactive)
  (let ((cpoint (point)))
    (paredit-backward-up)
    (sp-copy-sexp)
    (goto-char cpoint)))

(defun kill-surrounding-sexp ()
  (interactive)
  (paredit-backward-up)
  (sp-kill-sexp))

(defun copy-whole-buffer ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard!"))

;; USED?
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun new-empty-buffer ()
  "Create a new buffer called untitled"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun zoom-window--do-unzoom--no-color-change ()
  (require 'zoom-window)
  (let ((current-line (line-number-at-pos))
        (current-column (current-column))
        (current-buf (current-buffer)))
    ;;(zoom-window--restore-mode-line-face)
    (zoom-window--restore-window-configuration)
    (unless (string= (buffer-name current-buf) (buffer-name))
      (switch-to-buffer current-buf))
    (zoom-window--goto-line current-line)
    (move-to-column current-column)))

(defun zoom-window-no-color-change ()
  (interactive)
  (require 'zoom-window)
  (let ((enabled (zoom-window--enable-p))
        (curframe (window-frame nil)))
    (if (and (one-window-p) (not enabled))
        (message "There is only one window!!")
      (if enabled
          (with-demoted-errors "Warning: %S"
            (zoom-window--do-unzoom--no-color-change))
        ;; (zoom-window--save-mode-line-color)
        (zoom-window--save-buffers)
        (zoom-window--save-window-configuration)
        (delete-other-windows))
      (zoom-window--toggle-enabled))))

(defun buffer-modified-title-sign ()
  (if (buffer-modified-p (current-buffer)) "[...]  " ""))

(defun current-branch-name ()
  (require 'magit)
  (require 'subr-x)
  (if-let (branch (magit-git-string "status"))
    (concat "  .:.  " (car (last (split-string branch " "))))))

(defun toggle-comment ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (comment-or-uncomment-region (region-beginning) (region-end))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun xterm-title-update ()
  ;; https://www.emacswiki.org/emacs/FrameTitle
  "Sets buffer file name or buffer name as terminal title"
  (interactive)
  (require 'projectile)

  (if buffer-file-name
    (send-string-to-terminal (concat "\033]2; " (buffer-modified-title-sign) (buffer-file-name) (current-branch-name) "\007"))
    (send-string-to-terminal (concat "\033]2; " (buffer-name) "\007")))

  (if (not (equal (projectile-project-name) "-"))
    (send-string-to-terminal (concat "\033]1; (" (projectile-project-name) ") \007"))
    (send-string-to-terminal (concat "\033]1; " (buffer-name) "\007"))))

(defun copy-region-or-sexp ()
  (interactive)
  (require 'smartparens)
  (if (and transient-mark-mode mark-active)
      (kill-ring-save (region-beginning) (region-end))
    (sp-copy-sexp)))

(defun kill-region-or-sexp () ;; TODO don't move to clipboard
  (interactive)
  (require 'smartparens)
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (sp-kill-sexp)))

(defun paste-with-replace ()
  "Deletes selected region and pastes"
  (interactive)
  (when (and transient-mark-mode mark-active)
    (delete-active-region))
  (yank))

(defun paredit-delete-backward-or-region ()
  (require 'paredit)
  (interactive)
  (if (and transient-mark-mode mark-active)
    (paredit-delete-region (region-beginning) (region-end)) ;;(delete-active-region)
    (paredit-backward-delete)))

(defun paredit-kill-or-delete-region ()
  (require 'paredit)
  (interactive)
  (if (and transient-mark-mode mark-active)
    (delete-active-region) ;;(paredit-kill-region)
    (paredit-kill)))

(defun toggle-magit-status ()
  (interactive)
  (require 'subr-x)
  (if-let ((repo (magit-toplevel)))
      (if-let ((buf (get-buffer (concat "magit: " (car (last (split-string-and-unquote repo "/")))))))
          (if (get-buffer-window buf)
              (progn (delete-windows-on buf) (kill-buffer buf))
            (magit-status))
        (magit-status))
    (message "Not a git repo!")))

;; TODO maybe use paredit navigation?
(defun cider-eval-toplevel-sexp ()
  (interactive)
  (let ((curpoint (point)))
    (when (= (line-beginning-position) (line-end-position))
      (goto-char (cl-decf (point))))
    (when (< (point) (line-end-position))
      (goto-char (cl-incf (point))))
    (while (and
            (> (point) (line-beginning-position))
            (not
             (and
              ;; FIXME this is ugly
              (= 64 (char-after (line-beginning-position))) ;; @
              (= (point)  (+ 1 (line-beginning-position))))))
      (beginning-of-sexp))
    (cider-eval-sexp-at-point)
    (goto-char curpoint)))

(defun add-reframe-regs-to-imenu ()
  (add-to-list
   'imenu-generic-expression
   '("re-frame" "(*reg-\\(event-db\\|sub\\|sub-raw\\|fx\\|event-fx\\|event-ctx\\|cofx\\)[ \n]+\\([^\t \n]+\\)" 2)
   t))

(defun clojure/reset-reloaded-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (cider-interactive-eval
     "(reset)")))

(defun init/extensions ()
  (with-eval-after-load 'company
    (define-key company-active-map [left] #'company-abort))

  (add-to-list 'auto-mode-alist '("\\.html\\'" . sgml-mode))

  (require 'hideshow)
  (require 'sgml-mode)

  (add-to-list 'hs-special-modes-alist
               '(sgml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"

                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (add-hook 'sgml-mode-hook 'hs-minor-mode))

(defun init/git ()
  (require 'magit)
  (setq github-browse-file-show-line-at-point 1)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (setq magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (when (display-graphic-p)
    (define-key magit-status-mode-map (kbd "M-<tab>") 'next-multiframe-window))

  (global-set-key (kbd "M-# b") 'magit-blame)
  (global-set-key (kbd "s-b")   'magit-blame)
  (global-set-key (kbd "M-'")   'toggle-magit-status)
  (global-set-key (kbd "M-G")   'github-browse-file)
  (global-set-key (kbd "M-Z")   'magit-diff-buffer-file))

(defun init/multiple-cursors ()
  (require 'multiple-cursors)

  (defun mc-mark-line (direction)
   (mc/mark-lines 1 direction))

  (defun mc-cursors-on ()
    (interactive)
    (mc/maybe-multiple-cursors-mode))

  (defun mc-cursors-off ()
    (interactive)
    (mc/keyboard-quit))

  (defun mc-next-line ()
    (interactive)
    (mc-mark-line 'forwards))

  (defun mc-prev-line ()
    (interactive)
    (mc-mark-line 'backwards))

  (global-set-key (kbd "M-a")     'mc/mark-all-like-this)
  (global-set-key (kbd "M-w")     'mc/mark-next-like-this)
  (global-set-key (kbd "M-S-<down>") 'mc-next-line)
  (global-set-key (kbd "M-S-<up>") 'mc-prev-line)
  (global-set-key (kbd "M-|")  'mc-cursors-on)
  (global-set-key (kbd "M-\\") 'mc-cursors-off))

(defun clojure-hook ()
  (paredit-mode 1)
  (cider-mode 1)
  (rainbow-delimiters-mode 1)
  (highlight-parentheses-mode 1)
  (show-paren-mode 1)
  (hs-minor-mode 1)
  (yas-minor-mode 1)
  (clj-refactor-mode 1)

  (when (display-graphic-p)
    (define-key clojure-mode-map (kbd "s-r") 'hydra-cljr-help-menu/body)
    (define-key clojure-mode-map (kbd "C-<return>")  'cider-eval-buffer)
    (define-key clojure-mode-map (kbd "s-S-<return>") 'cider-eval-defun-to-comment)
    (define-key clojure-mode-map (kbd "s-<return>")   'cider-eval-toplevel-sexp)
    (define-key clojure-mode-map (kbd "S-<return>")  'cider-eval-sexp-at-point))

  (define-key clojure-mode-map (kbd "M-# r") 'hydra-cljr-help-menu/body)
  (define-key clojure-mode-map (kbd "M->") 're-frame-jump-to-reg)

  (define-key clojure-mode-map (kbd "M-RET '") 'cider-jack-in)
  (define-key clojure-mode-map (kbd "M-RET ;") 'cider-jack-in-cljs)
  (define-key clojure-mode-map (kbd "M-RET t a") 'cider-test-run-loaded-tests)
  (define-key clojure-mode-map (kbd "M-RET t n") 'cider-test-run-ns-tests)
  (define-key clojure-mode-map (kbd "M-RET t t") 'cider-test-run-test)
  (define-key clojure-mode-map (kbd "M-RET s s") 'cider-switch-to-repl-buffer)
  (define-key clojure-mode-map (kbd "M-RET /")   'clojure/reset-reloaded-repl)
  (define-key clojure-mode-map (kbd "M-RET h h") 'cider-doc)

  (define-key clojure-mode-map (kbd "M-# *!!")  'cider-eval-buffer)
  (define-key clojure-mode-map (kbd "M-# #_!!") 'cider-eval-defun-to-comment)
  (define-key clojure-mode-map (kbd "M-# !!")   'cider-eval-toplevel-sexp)
  (define-key clojure-mode-map (kbd "M-# _!!")  'cider-eval-sexp-at-point) ;; TODO - good combination
  (define-key clojure-mode-map (kbd "M-i") 'cider-inspect-last-result)
  (define-key clojure-mode-map (kbd "RET") 'newline-and-indent) ;; paredit-newline does too much autoident

  (define-key cider-inspector-mode-map (kbd "M-<left>") 'cider-inspector-prev-page)
  (define-key cider-inspector-mode-map (kbd "M-<right>")'cider-inspector-next-page)
  (define-key cider-inspector-mode-map (kbd "M-<up>") 'cider-inspector-pop)
  (define-key cider-inspector-mode-map (kbd "M-<down>") 'cider-inspector-operate-on-point)

  (put-clojure-indent 'reg-sub 1)
  (put-clojure-indent 'reg-fx 1)
  (put-clojure-indent 'reg-cofx 1)
  (put-clojure-indent 'reg-event-fx 1)
  (put-clojure-indent 'reg-event-db 1))

(defun init/lisp ()
  (setq cljr-warn-on-eval nil)
  (setq imenu-auto-rescan t)
  (setq cider-repl-display-in-current-window t)
  (setq cider-eval-result-duration 15)
  (setq cider-prompt-for-symbol nil)
  (setq cider-cljs-lein-repl
        "(do
         (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")

  (add-hook 'clojure-mode-hook #'clojure-hook)

  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook #'show-paren-mode)
  (add-hook 'emacs-lisp-mode-hook #'hs-minor-mode)
  (add-hook 'clojurescript-mode-hook #'add-reframe-regs-to-imenu)

  (define-key emacs-lisp-mode-map (kbd "RET") 'paredit-newline)
  (define-key emacs-lisp-mode-map (kbd "M-# !!") 'eval-last-sexp)
  (define-key emacs-lisp-mode-map (kbd "s-<return>") 'eval-last-sexp)

  (require 'expand-region)
  (global-set-key (kbd "M-e") 'er/expand-region)
  (global-set-key (kbd "M-o") 'er/mark-outside-pairs)

  (require 'paredit)
  ;; TODO paredit-kill should not move into clipboard
  (define-key paredit-mode-map (kbd "M-<up>")    'paredit-backward-up)
  (define-key paredit-mode-map (kbd "M-<down>")  'paredit-forward-down)
  (define-key paredit-mode-map (kbd "M-<left>")  'paredit-backward)
  (define-key paredit-mode-map (kbd "M-<right>") 'paredit-forward)

  (define-key paredit-mode-map (kbd "DEL") 'paredit-delete-backward-or-region)
  (define-key paredit-mode-map (kbd "M-DEL") 'paredit-backward-delete-line)
  (define-key paredit-mode-map (kbd "M-# %%") 'paredit-kill-or-delete-region)
  (define-key paredit-mode-map (kbd "s-<backspace>") 'paredit-kill-or-delete-region)

  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-9") 'paredit-wrap-round)
  (define-key paredit-mode-map (kbd "M-0") 'paredit-close-round)
  (define-key paredit-mode-map (kbd "M-?") 'helm-imenu-in-all-buffers) ;; more useful than convolute-sexp
  (define-key paredit-mode-map (kbd "M-# +[") 'paredit-wrap-square)
  (when (display-graphic-p)
    (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)))

(defun init/clipboard ()
  (defun copy-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun paste-from-osx ()
    (shell-command-to-string "pbpaste"))

  (setq interprogram-cut-function 'copy-to-osx)
  (setq interprogram-paste-function 'paste-from-osx))

(defun init/mappings ()
  (define-key input-decode-map "\e[1;10A" [S-M-up])
  (define-key input-decode-map "\e[1;10B" [S-M-down])
  (define-key input-decode-map "\e[1;10C" [S-M-right])
  (define-key input-decode-map "\e[1;10D" [S-M-left])
  (define-key input-decode-map "\e[1;9A"  [M-up])
  (define-key input-decode-map "\e[1;9B"  [M-down])
  (define-key input-decode-map "\e[1;9C"  [M-right])
  (define-key input-decode-map "\e[1;9D"  [M-left])
  (define-key input-decode-map "\e[1;8A"  [C-M-up])
  (define-key input-decode-map "\e[1;8B"  [C-M-down])
  (define-key input-decode-map "\e[1;8C"  [C-M-right])
  (define-key input-decode-map "\e[1;8D"  [C-M-left])

  ;; (define-key input-decode-map "\e[1;5A" [C-up])
  ;; (define-key input-decode-map "\e[1;5B" [C-down])
  ;; (define-key input-decode-map "\e[1;5C" [C-right])
  ;; (define-key input-decode-map "\e[1;5D" [C-left])
  ;; (define-key input-decode-map "\e[1;6A" [C-S-up])
  ;; (define-key input-decode-map "\e[1;6B" [C-S-down])
  ;; (define-key input-decode-map "\e[1;6C" [C-S-right])
  ;; (define-key input-decode-map "\e[1;6D" [C-S-left])
  )

;; ---------KEYS IN ESC SEQ------------
;; #   cmd
;; _   shift
;; __  tab
;; *   ctrl
;; +   alt
;; !!  enter
;; %%  delete
;; @>  arrow right
;; @<  arrow left
;; @v  arrow down
;; @^  arrow up
;; ---------KEYS IN ESC SEQ------------

(defun init/keybindings ()
  ;; basic
  (global-set-key (kbd "M-c")     'copy-region-or-sexp)
  (global-set-key (kbd "M-C")     'copy-surrounding-sexp)
  (global-set-key (kbd "M-# D")   'sp-clone-sexp-noindent)
  (global-set-key (kbd "M-# V")   'paste-sexp-with-replace)
  (global-set-key (kbd "M-# v")   'paste-with-replace)
  (global-set-key (kbd "M-# x")   'kill-region-or-sexp)
  (global-set-key (kbd "M-# X")   'kill-surrounding-sexp)
  (global-set-key (kbd "M-# a")   'copy-whole-buffer)
  (global-set-key (kbd "M-# A")   'mark-whole-buffer)

  (require 'undo-tree)
  (global-set-key (kbd "M-# z")   (lambda () (interactive) (deactivate-mark) (undo-tree-undo)))
  (global-set-key (kbd "M-# Z")   (lambda () (interactive) (deactivate-mark) (undo-tree-redo)))

  (global-set-key (kbd "M-# s")   'save-buffer) ;; TODO clean selection
  (global-set-key (kbd "M-# R")   'revert-buffer-no-confirm)
  (global-set-key (kbd "M-`")     'keyboard-quit)
  (global-set-key (kbd "M-# /")   'toggle-comment)
  (global-set-key (kbd "M-# }")   'indent-rigidly-right)
  (global-set-key (kbd "M-# {")   'indent-rigidly-left)
  (global-set-key (kbd "M-# d")   'duplicate-line-or-region)
  (global-set-key (kbd "M-# l")   'delete-space-forward)
  (global-set-key (kbd "M-# k")   'delete-space-backward)

  ;; search
  (require 'ivy)
  (require 'helm-projectile)
  (global-set-key (kbd "M-# \"")  'ivy-resume)
  (global-set-key (kbd "M-# S")   'swiper)
  (global-set-key (kbd "M-# +S")  'counsel-projectile-ag)
  (define-key ivy-minibuffer-map (kbd "M-`") 'kill-this-buffer)
  (global-set-key (kbd "M-# g")   'rgrep)
  (global-set-key (kbd "M-# .")   'search-symbol-at-point)
  (global-set-key (kbd "M-# f")   'helm-do-ag-this-file)
  (global-set-key (kbd "M-# F")   'helm-projectile-ag)
  (global-set-key (kbd "M-# p")   'helm-projectile-find-file)
  (global-set-key (kbd "M-/")     'helm-semantic-or-imenu)
  (global-set-key (kbd "M-?")     'helm-imenu-in-all-buffers)
  (global-set-key (kbd "M-f")     'find-file)

  ;; tools
  (with-eval-after-load "neotree"
    (define-key neotree-mode-map [(left)] 'neotree-select-up-node)
    (define-key neotree-mode-map [(right)] 'neotree-enter)
    (define-key neotree-mode-map (kbd "TAB") 'neotree-stretch-toggle))
  (global-set-key (kbd "M-# |")   'neotree-toggle)

  ;; windows/buffers management
  (global-set-key (kbd "M-+ __")  'next-multiframe-window)
  (global-set-key (kbd "M-# q")   'kill-buffer-and-window)
  (global-set-key (kbd "M-# w")   'delete-window)
  (global-set-key (kbd "M-# +@v") 'next-buffer)
  (global-set-key (kbd "M-# +@^") 'previous-buffer)
  (global-set-key (kbd "M-z")     'zoom-window-no-color-change)
  (global-set-key (kbd "M-# n")   'new-empty-buffer)
  (global-set-key (kbd "M-# N")   (lambda () (interactive) (new-empty-buffer) (clojure-mode)))

  ;; navigation/selection
  (global-set-key (kbd "M-m") 'set-mark-command)
  (global-set-key (kbd "M-# */") 'set-mark-command)
  (global-set-key (kbd "M-# @^")  'backward-paragraph)
  (global-set-key (kbd "M-# @v") 'forward-paragraph)

  (global-set-key (kbd "C-M-<up>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "C-M-<down>") 'scroll-up-line) ;; TODO off all mc / selection

  (global-set-key (kbd "M-# @>")  'end-of-visual-line)
  (global-set-key (kbd "M-# @<")  'beginning-of-line-text)

  (global-set-key (kbd "M-# *@<")  'backward-word)
  (global-set-key (kbd "M-# *@>")  'forward-word)

  (global-set-key (kbd "C-g")     'goto-line)
  (global-set-key (kbd "M-# j")   'bookmark-set)
  (global-set-key (kbd "M-# J")   'bookmark-bmenu-list)

  ;; helm
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-# ~")   'helm-buffers-list)
  (global-set-key (kbd "M-* __")  'helm-buffers-list)
  (global-set-key (kbd "M-# '")   'helm-resume)
  (global-set-key (kbd "C-t")     'helm-themes)
  (require 'helm-buffers)
  (add-hook 'helm-after-initialize-hook
            (lambda()
              (define-key helm-buffer-map (kbd "TAB") 'helm-execute-persistent-action)
              (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
              (define-key helm-buffer-map (kbd "M-`") 'helm-keyboard-quit)
              (define-key helm-map (kbd "M-`") 'helm-keyboard-quit)))

  ;; hide/show
  (global-set-key (kbd "C-\\")    'hs-toggle-hiding)
  (global-set-key (kbd "M-F h")   'hs-hide-all)
  (global-set-key (kbd "M-F s")   'hs-show-all))

(defun init/keybindings-ui ()
  ;; basic
  (global-unset-key (kbd "ESC ESC ESC"))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (global-set-key (kbd "s-c") 'copy-region-or-sexp)
  (global-set-key (kbd "s-C") 'copy-surrounding-sexp)
  (global-set-key (kbd "s-D") 'sp-clone-sexp-noindent)
  (global-set-key (kbd "s-V") 'paste-sexp-with-replace)
  (global-set-key (kbd "s-v") 'paste-with-replace)
  (global-set-key (kbd "s-x") 'kill-region-or-sexp)
  (global-set-key (kbd "s-X") 'kill-surrounding-sexp)
  (global-set-key (kbd "s-a") 'copy-whole-buffer)
  (global-set-key (kbd "s-A") 'mark-whole-buffer)
  (global-set-key (kbd "s-s") 'save-buffer) ;; TODO clean selection
  (global-set-key (kbd "s-R") 'revert-buffer-no-confirm)
  (global-set-key (kbd "s-/") 'toggle-comment)
  (global-set-key (kbd "s-}") 'indent-rigidly-right)
  (global-set-key (kbd "s-{") 'indent-rigidly-left)
  (global-set-key (kbd "s-d") 'duplicate-line-or-region)
  (global-set-key (kbd "s-l") 'delete-space-forward)
  (global-set-key (kbd "s-k") 'delete-space-backward)

  ;; search
  (global-set-key (kbd "s-\"")  'ivy-resume)
  (global-set-key (kbd "s-S")   'swiper)
  (global-set-key (kbd "s-M-S") 'counsel-projectile-ag)
  (global-set-key (kbd "s-g")   'rgrep)
  (global-set-key (kbd "s-.")   'search-symbol-at-point)
  (global-set-key (kbd "s-f")   'helm-do-ag-this-file)
  (global-set-key (kbd "s-F")   'helm-projectile-ag)
  (global-set-key (kbd "s-p")   'helm-projectile-find-file)

  ;; tools
  (global-set-key (kbd "s-\\") 'neotree-toggle)

  ;; windows/buffers management
  (global-set-key (kbd "s-ESC") 'keyboard-quit)
  (global-set-key (kbd "M-<tab>")  'next-multiframe-window)
  (global-set-key (kbd "s-q")   'kill-buffer-and-window)
  (global-set-key (kbd "s-w")   'delete-window)
  (global-set-key (kbd "s-M-<down>") 'next-buffer)
  (global-set-key (kbd "s-M-<up>") 'previous-buffer)
  (global-set-key (kbd "s-n")   'new-empty-buffer)
  (global-set-key (kbd "S-N")   (lambda () (interactive) (new-empty-buffer) (clojure-mode)))

  ;; navigation/selection
  (global-set-key (kbd "s-C-/") 'set-mark-command)
  (global-set-key (kbd "s-<up>")  'backward-paragraph)
  (global-set-key (kbd "s-<down>") 'forward-paragraph)
  (global-set-key (kbd "C-M-<up>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "C-M-<down>") 'scroll-up-line) ;; TODO off all mc / selection
  (global-set-key (kbd "s-<right>")  'end-of-visual-line)
  (global-set-key (kbd "s-<left>")  'beginning-of-line-text)
  (global-set-key (kbd "s-C-<left>")  'backward-word)
  (global-set-key (kbd "s-C-<right>")  'forward-word)

  ;; perspective
  (global-set-key (kbd "M-s-<right>")  'persp-next)
  (global-set-key (kbd "M-s-<left>")  'persp-prev)
  (global-set-key (kbd "C-`")  'persp-switch)

  ;; USED??
  (global-set-key (kbd "s-j")   'bookmark-set)
  (global-set-key (kbd "s-J")   'bookmark-bmenu-list)

  ;; helm
  (global-set-key (kbd "s-`")   'helm-buffers-list)
  (global-set-key (kbd "C-<tab>")  'helm-buffers-list)
  (global-set-key (kbd "s-'")   'helm-resume)
)

(defun init/ui ()
  (require 'projectile)
  (set-default-font "CamingoCode 17")
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (global-visual-line-mode 1)
  (when (display-graphic-p) (scroll-bar-mode -1))
  (setq-default mode-line-format (list " " mode-line-modified  " %b "
                                       '(:eval (when (ignore-errors (projectile-project-root)) (concat "[" (projectile-project-name) "]")))
                                       " === %l:%p {%m}" cider-mode-line))
  (setq base16-theme-256-color-source 'colors)
  (load-theme 'nord t)
  (fset 'yes-or-no-p 'y-or-n-p))

(defun init/modes ()
  (setq indent-tabs-mode nil)
  (setq company-auto-complete t)
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  ;; don't make autoident on newline
  (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

  (setq create-lockfiles nil)
  (setq backup-directory-alist
        '(("" . "~/.emacs.d/backup")))
  (setq auto-save-file-name-transforms
        `((".*" "~/.emacs.d/backup/" t)))

  (setq neo-smart-open t)
  (setq-default neo-show-hidden-files t)
  ;; helm
  (setq helm-always-two-windows nil)
  (setq helm-display-buffer-default-height 15)
  (setq helm-default-display-buffer-functions '(display-buffer-in-side-window))
  (setq-default helm-M-x-fuzzy-match t)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  (helm-mode 1)
  (global-company-mode 1)
  (global-eldoc-mode 1)
  (global-highlight-parentheses-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 0)
  (which-key-mode 1))

(defun init/hooks ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook
            (lambda ()
              (interactive)
              (untabify (point-min) (point-max))))
  (if (not (display-graphic-p))
      (add-hook 'post-command-hook 'xterm-title-update)))

(defun init/setup ()
  (init/packages)
  (init/clipboard)
  (init/ui)
  (init/mappings)
  (init/keybindings)
  (when (display-graphic-p)
    (init/keybindings-ui))
  (init/modes)
  (init/hooks)
  (init/git)
  (init/multiple-cursors)
  (init/lisp)
  (init/extensions))

(init/setup)
