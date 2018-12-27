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
          ))

  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun new-empty-buffer ()
  "Create a new buffer called untitled"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

(defun init/lisp ()
		(add-hook 'clojure-mode-hook #'paredit-mode)
		(add-hook 'clojure-mode-hook #'cider-mode)
		(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
)

(defun init/languages ()
		(init/lisp)
		)

(defun init/keybindings ()
  ;; basic
  (global-set-key (kbd "M-# s")   'save-buffer)
		(global-set-key (kbd "M-`")     'keyboard-quit)

  ;; windows/buffers management
  (global-set-key (kbd "M-+ __")  'next-multiframe-window)
  (global-set-key (kbd "M-# q")   'kill-buffer-and-window)
  (global-set-key (kbd "M-# w")   'delete-window)
  (global-set-key (kbd "M-# +@v") 'next-buffer)
  (global-set-key (kbd "M-# +@^") 'previous-buffer)
  ;;(global-set-key (kbd "M-z")     'zoom-window-no-color-change)
  (global-set-key (kbd "M-# n")   'new-empty-buffer)
  (global-set-key (kbd "M-# N")   (lambda () (interactive) (new-empty-buffer) (clojure-mode)))

  ;; helm
  (global-set-key (kbd "M-x")     'helm-M-x)
  (global-set-key (kbd "M-# ~")   'helm-buffers-list)
  (global-set-key (kbd "M-# '")   'helm-resume)
  (global-set-key (kbd "C-t")     'helm-themes)
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
  ;;(add-hook 'post-command-hook 'xterm-title-update)
  )

(defun init/setup ()
  (init/packages)
  (init/ui)
  (init/keybindings)
  (init/modes)
  (init/hooks)
		(init/languages)
  )

(init/setup)
