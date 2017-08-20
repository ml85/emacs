(package-initialize)

;;* General initialization
;;** General start up
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin") exec-path))
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq-default truncate-lines t)
(setq column-number-mode t)
(delete-selection-mode t)
(show-paren-mode 1)
(electric-pair-mode)
(global-linum-mode 1)
(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)
;;(set-default-font "Inconsolata-10")

;;** Remove toolbar, scrollbar and menubar
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;;* Highlight current line
(global-hl-line-mode 1)

;;** Disable autosave and backups
(setq make-backup-files nil)
(setq auto-save-default nil)
(auto-save-mode 0)

;;** Formatting
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)
(setq-default javascript-indent-level 2)
(setq-default js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq-default css-indent-offset 2)

(load "~/.emacs.d/google-c-style.el")

;;** Inherit bash environment.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;** Theme
(load-theme 'leuven t)

;;* Packages
;;** Load path
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags")

;;** Package archives
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

;;** General package initialization
(require 'use-package)
(require 'cl)
(require 'cc-mode)

;;** Fix colors in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;** Evil mode
;; (use-package evil
;;   :init
;;   (setq evil-want-C-u-scroll t)
;;   :config
;;   (evil-mode 1)

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
	 ("C->" . mc/mark-next-like-this-word)
	 ("C-<" . mc/mark-previous-like-this-word)
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

;;*** Ag
(use-package ag)

;;*** Projectile
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching nil)
  (setq projectile-project-root-files-bottom-up '(".projectile"))
  (setq projectile-project-root-files-functions '(projectile-root-bottom-up))
  :config
  (progn
    ;; Use `ag' all the time if available
    (defun mkiael/advice-projectile-use-ag ()
      "Always use `ag' for getting a list of all files in the project."
      (mapconcat 'identity
                 (append '("\\ag") ; used unaliased version of `ag': \ag
                         '("--nogroup" ;mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
                           "--skip-vcs-ignores" ;Ignore files/dirs ONLY from `.ignore'
                           "--numbers" ;Line numbers
                           "--smart-case"
                           "--follow" ;Follow symlinks
                           "--ignore" "build" )
                         '("-0" ; output null separated results
                           "-g ''")) ; get file names matching the regex '' (all files)
                 " "))
    (when (executable-find "ag")
      (advice-add 'projectile-get-ext-command :override #'mkiael/advice-projectile-use-ag))

    ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
    (defun mkiael/advice-projectile-no-sub-project-files ()
      "Directly call `projectile-get-ext-command'. No need to try to get a list of sub-project files if the vcs is git."
      (projectile-files-via-ext-command (projectile-get-ext-command)))
    (advice-add 'projectile-get-repo-files :override #'mkiael/advice-projectile-no-sub-project-files)))

;;*** Encryption
;;(use-package epa-file
;;  :init
;;  (epa-file-enable))

;;** Programming
;;*** General
;;(use-package sr-speedbar
;;  :init
;;  (setq speedbar-use-images nil))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(use-package company
  :init
  (global-company-mode)
  :config
  (delete 'company-backends 'company-clang))

;; To get a bunch of extra snippets that come in super handy see:
;; htps://github.com/AndreaCrotti/yasnippet-snippets
;; or use:
;; git clone https://github.com/AndreaCrotti/yasnippet-snippets.git ~/.emacs.d/yassnippet-snippets/
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets/")
  (add-to-list 'company-backends 'company-yasnippet)
  (yas-reload-all))

;;*** C++
;;(use-package clang-format
;;  :bind (("C-c i" . clang-format-buffer)))

;;(require 'cmake-ide)
;;(cmake-ide-setup)
;;(setq cmake-ide-flags-c++ (append '("-std=c++14")))
;;(setq cmake-ide-dir "build")
;;(global-set-key (kbd "C-c m") 'cmake-ide-compile)

(use-package rtags
  :init
  (setq rtags-path "/usr/local/bin")
  (setq rtags-display-result-backend 'ivy)
  (setq rtags-autostart-diagnostics t)
  :config
  (rtags-enable-standard-keybindings))
(rtags-diagnostics)

(use-package irony
  :commands irony-mode
  :config
  (setq irony-additional-clang-options '("-std=c++11"))
  (use-package company-irony
    :config
    (add-to-list 'company-backends 'company-irony))
  (use-package company-irony-c-headers
    :config
    (add-to-list 'company-backends 'company-irony-c-headers))
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map
      [remap completion-at-point] 'irony-completion-at-point-async)
    (define-key irony-mode-map
      [remap complete-symbol] 'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package flycheck
  :config
  (use-package flycheck-rtags))

(defun my-flycheck-rtags-setup ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil)
      (setq-local flycheck-check-syntax-automatically nil))

(defun my-c-mode-common-hook ()
  (rtags-start-process-unless-running)
  (irony-mode)
  (flycheck-mode)
  (my-flycheck-rtags-setup)
  (google-set-c-style)
  (google-make-newline-indent))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(global-set-key [f12] 
  		'(lambda () 
  		   (interactive)
  		   (if (buffer-file-name)
  		       (let*
  			   ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
  			    (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H" "\n"))
  			    (begin (point-marker))
  			    )
  			 (progn
  					; If less then 5 characters are in the buffer, insert the class definition
  			   (if (< (- (point-max) (point-min)) 5 )
  			       (progn
  				 (insert "\nclass " (capitalize fName) "{\npublic:\n\nprivate:\n\n};\n")
  				 (goto-char (point-min))
  				 (next-line-nomark 3)
  				 (setq begin (point-marker))
  				 )
  			     )
  			   
  					;Insert the Header Guard
  			   (goto-char (point-min))
  			   (insert ifDef)
  			   (goto-char (point-max))
  			   (insert "\n#endif" " //" fName "_H")
  			   (goto-char begin))
  			 )
                                        ;else
  		     (message (concat "Buffer " (buffer-name) " must have a filename"))
  		     )
  		   )
  		)

;;*** Haskell
;;(use-package haskell-mode)

;;*** Python
(use-package elpy
  :init
  (elpy-enable))

;;*** Yaml
;;(use-package yaml-mode)

;;*** Javascript
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config (setq-default
           js2-strict-missing-semi-warning nil
           js2-basic-offset 2
           js2-bounce-indent-p t))

(use-package js2-refactor
  :diminish js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

;;* Key bindings
(global-set-key (kbd "C-c C-l") 'goto-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "s-<left>") 'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>") 'windmove-up)
(global-set-key (kbd "s-<down>") 'windmove-down)
(global-set-key (kbd "C-c C-k") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "M-g M-s") 'magit-status)
(global-set-key (kbd "M-g M-c") 'magit-checkout)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key "\C-c\C-y" "\C-a\C- \C-n\M-w\C-y")
