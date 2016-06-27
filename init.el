;; hide menu bar, scrollbar and toolbar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; do not show default startup screen
(setq inhibit-startup-message t)

;; highlight matching paranthesis
(show-paren-mode 1)

;; do not create annoying backup files
(setq make-backup-files nil)

;; turn off auto save mode
(auto-save-mode 0)

;; show line number
(global-linum-mode 1)

(require 'package)

(setq package-list
      '(avy
        leuven-theme
	magit
        swiper
	))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
     			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(package-refresh-contents)

(dolist (package package-list)
  (unless (package-installed-p package)
    (message "Installing %s" package)
    (package-install package)))

(org-babel-load-file (expand-file-name "emacs.org" user-emacs-directory))
