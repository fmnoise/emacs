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
	  helm
          ))

  (dolist (pkg my-packages)
    (unless (package-installed-p pkg)
      (package-install pkg))))

(defun init/keybindings ()
  ;; setup keybindings here
  )

(defun init/ui ()
  (load-theme 'zenburn t)
  (menu-bar-mode -1)
  )

(defun init/setup ()
  (init/packages)
  (init/ui)
  (init/keybindings)
  ;; your init calls here
  )

(init/setup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (clojure-mode zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
