(package-initialize)

;;* General initialization
;;** General start up
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin") exec-path))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq-default truncate-lines t)
(delete-selection-mode t)
(show-paren-mode 1)
(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-default-font "Inconsolata-10")

;;** Remove toolbar, scrollbar and menubar
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;;** Disable autosave and backups
(setq make-backup-files nil)
(setq auto-save-default nil)
(auto-save-mode 0)

;;** Formatting
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 3)

;;** Theme
(setq custom-safe-themes t)
;;(require 'zenburn-theme)
;;(load-theme 'zenburn t)
;;(require 'leuven-theme)
;;(load-theme 'leuven t)
(require 'solarized-theme)
(load-theme 'solarized-dark t)

;;* Packages
;;** Package archives
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

;;** General package initialization
(require 'use-package)
(require 'cl)
(require 'cc-mode)

;;** Cursor movement
;;*** Avy             
(use-package avy
  :init
  (setq avy-background t)
  :bind (("C-'" . avy-goto-char)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)))

;;*** Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;;** Select, erase etc
;;*** Change inner
(use-package change-inner
  :bind (("M-i" . change-inner)
	 ("M-o" . change-outer)))

;;*** Expand region
(use-package expand-region
  :bind (("C-=" . er/expand-region)))

;;** Dired
(use-package dired
  :init
  (toggle-diredp-find-file-reuse-dir 1)
  (setq dired-hide-details-hide-information-lines nil)
  (setq dired-hide-details-hide-symlink-targets nil)
  (setq dired-listing-switches "-alh"))

;;*** Dired-subtree
(use-package dired-subtree
  :init
  (setq dired-subtree-use-backgrounds nil)
  (setq dired-subtree-line-prefix-face nil)
  :bind (:map dired-mode-map
	      ("i" . dired-subtree-toggle)))

;;** Neotree
(use-package neotree
  :bind ("<f8>" . neotree-toggle))

;;** Search and completion
;;*** Ivy
(use-package ivy
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  :bind (("C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

;;*** Projectile
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil))

;;*** Encryption
;;(use-package epa-file
;;  :init
;;  (epa-file-enable))

;;** Programming
;;*** General
(use-package sr-speedbar
  :init
  (setq speedbar-use-images nil))

;;*** C++
(use-package clang-format
  :bind (("C-c i" . clang-format-buffer)))

(use-package rtags
  :init
  (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-common-hook 'rtags-start-process-unless-running)
  (setq rtags-path "/usr/local/bin")
  (setq rtags-autostart-diagnostics t)
  (setq rtags-completions-enabled t)
  :config
  (rtags-enable-standard-keybindings))
  (rtags-diagnostics)

;;(use-package company
;;  :init
;;  (global-company-mode)
;;  :config
;;  (push 'company-rtags company-backends))
;;(define-key c-mode-base-map (kbd "C-TAB") (function company-complete))

;;*** Haskell
(use-package haskell-mode)

;;*** Python
(use-package elpy
  :init
  (elpy-enable))

;;*** Yaml
(use-package yaml-mode)

;;* Key bindings
(global-set-key (kbd "C-c C-l") 'goto-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
(global-set-key (kbd "s-<down>")  'windmove-down)
