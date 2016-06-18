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

(setq package-list '(evil))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'evil)
(evil-mode 1)

(setq custom-safe-themes t)

(load-theme 'leuven t)
