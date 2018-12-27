(defun new-empty-buffer ()
  "Create a new buffer called untitled"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun zoom-window-no-color-change ()
  (interactive)
  (require 'zoom-window)
  (let ((enabled (zoom-window--enable-p))
        (curframe (window-frame nil)))
    (if (and (one-window-p) (not enabled))
        (message "There is only one window!!")
      (if enabled
          (with-demoted-errors "Warning: %S"
            (zoom-window--do-unzoom))
        (zoom-window--save-mode-line-color)
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

(defun toggle-magit-status ()
  (interactive)
  (if-let ((repo (magit-toplevel)))
      (if-let ((buf (get-buffer (concat "magit: " (car (last (split-string-and-unquote repo "/")))))))
          (if (get-buffer-window buf)
              (progn (delete-windows-on buf) (kill-buffer buf))
            (magit-status))
        (magit-status))
    (message "Not a git repo!")))

(defun init/packages ()
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("marmalade" . "http://marmalade-repo.org/packages/")
          ("melpa" . "http://melpa.org/packages/")))

  (require 'package)
  (package-initialize)
  ;;(package-refresh-contents) ;; comment later to

  (setq my-packages
        '(
          zenburn-theme
          clojure-mode
          company
          cider
          paredit
          smartparens
          helm
          rainbow-mode
          magit
          plan9-theme
										zoom-window
										helm-themes
										helm-ag
										helm-projectile
										neotree
										projectile
										undo-tree
          ))

  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun init/git ()
  (require 'magit)
  ;;(setq github-browse-file-show-line-at-point 1)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (global-set-key (kbd "M-# b") 'magit-blame)
  (global-set-key (kbd "M-'")   'toggle-magit-status)
  ;;(global-set-key (kbd "M-G")   'github-browse-file)
  (global-set-key (kbd "M-Z")   'magit-diff-buffer-file) ;;???
  )

(defun init/lisp ()
		(add-hook 'clojure-mode-hook #'paredit-mode)
		(add-hook 'clojure-mode-hook #'cider-mode)
		(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
		)

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

(defun init/languages ()
		(init/lisp)
		)

(defun init/keybindings ()
  ;; basic
  (global-set-key (kbd "M-c")     'copy-region-or-sexp)
		;; (global-set-key (kbd "M-C")     'copy-surrounding-sexp)
		;; (global-set-key (kbd "M-# D")   'sp-clone-sexp-noindent)
  ;; (global-set-key (kbd "M-# V")   'paste-sexp-with-replace)
  (global-set-key (kbd "M-# v")   'paste-with-replace)
  (global-set-key (kbd "M-# x")   'kill-region-or-sexp)
  ;; (global-set-key (kbd "M-# X")   'kill-surrounding-sexp)
  ;; (global-set-key (kbd "M-# a")   'copy-whole-buffer)
  ;; (global-set-key (kbd "M-# A")   'mark-whole-buffer)

		(require 'undo-tree)
  (global-set-key (kbd "M-# z")   (lambda () (interactive) (deactivate-mark) (undo-tree-undo)))
  (global-set-key (kbd "M-# Z")   (lambda () (interactive) (deactivate-mark) (undo-tree-redo)))

  (global-set-key (kbd "M-# s")   'save-buffer) ;; TODO clean selection
  (global-set-key (kbd "M-`")     'keyboard-quit)
  (global-set-key (kbd "M-# /")   'toggle-comment)
  (global-set-key (kbd "M-# }")   'indent-rigidly-right)
  (global-set-key (kbd "M-# {")   'indent-rigidly-left)
  ;; (global-set-key (kbd "M-# d")   'duplicate-line-or-region)
  ;; (global-set-key (kbd "M-# l")   'delete-space-forward)
  ;; (global-set-key (kbd "M-# k")   'delete-space-backward)

  ;; search
  ;; (require 'ivy)
  ;; (global-set-key (kbd "M-# \"")  'ivy-resume)
  ;; (global-set-key (kbd "M-# S")   'swiper)
  ;; (global-set-key (kbd "M-# +S")  'counsel-projectile-ag)
  ;; (define-key ivy-minibuffer-map (kbd "<escape>") 'kill-this-buffer)
  ;; (global-set-key (kbd "M-# g")   'rgrep)
  ;; (global-set-key (kbd "M-# .")   'search-symbol-at-point)
  (global-set-key (kbd "M-# f")   'helm-do-ag-this-file)
  (global-set-key (kbd "M-# F")   'helm-projectile-ag)
  (global-set-key (kbd "M-# p")   'helm-projectile-find-file)
  (global-set-key (kbd "M-/")     'helm-semantic-or-imenu)
  (global-set-key (kbd "M-?")     'helm-imenu-in-all-buffers)

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

  ;; navigation
  (global-set-key (kbd "M-# @^")  'backward-paragraph)
  (global-set-key (kbd "M-# @v") 'forward-paragraph)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line) ;; TODO off all mc / selection
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "C-M-<up>") 'scroll-down-line) ;; TODO off all mc / selection
  (global-set-key (kbd "C-M-<down>") 'scroll-up-line) ;; TODO off all mc / selection
  (global-set-key (kbd "M-# *@>") 'end-of-visual-line) ;; TODO dupe, decide which is better
  (global-set-key (kbd "M-# *@<") 'beginning-of-line-text) ;; TODO dupe, decide which is better
  (global-set-key (kbd "M-# _@>") 'end-of-visual-line)
  (global-set-key (kbd "M-# _@<") 'beginning-of-line-text)
  (global-set-key (kbd "M-# @<")  'backward-word)
  (global-set-key (kbd "M-# @>")  'forward-word)
  (global-set-key (kbd "C-g")     'goto-line)
  (global-set-key (kbd "M-# j")   'bookmark-set)
  (global-set-key (kbd "M-# J")   'bookmark-bmenu-list)

  ;; helm
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-# ~")   'helm-buffers-list)
  (global-set-key (kbd "M-# '")   'helm-resume)
  (global-set-key (kbd "C-t")     'helm-themes)
		(require 'helm-buffers)
		(add-hook 'helm-after-initialize-hook
												(lambda()
														(define-key helm-buffer-map (kbd "`") 'helm-keyboard-quit)
														(define-key helm-map (kbd "`") 'helm-keyboard-quit)))
  )

(defun init/ui ()
  (load-theme 'zenburn t)
  (menu-bar-mode -1)
  )


(defun init/modes ()
  (setq indent-tabs-mode nil)
  (setq company-auto-complete t)
  (setq require-final-newline nil)
  (setq mode-require-final-newline nil)
  ;; don't make autoident on newline
  (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
  (setq-default tab-width 1)

		;; helm
		(setq helm-always-two-windows nil)
		(setq helm-display-buffer-default-height 23)
		(setq helm-default-display-buffer-functions '(display-buffer-in-side-window))

  (global-company-mode 1)
  (global-eldoc-mode 1)
  (delete-selection-mode 1)
  (savehist-mode 0)
  )

(defun init/hooks ()
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'before-save-hook
            (lambda () (if (not indent-tabs-mode)
                           (untabify (point-min) (point-max)))
              nil))
  (add-hook 'post-command-hook 'xterm-title-update)
  )

(defun init/setup ()
  (init/packages)
		(init/clipboard)
  (init/ui)
  (init/keybindings)
  (init/modes)
  (init/hooks)
		(init/git)
		(init/languages)
  )

(init/setup)
