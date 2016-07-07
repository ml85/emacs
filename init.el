(require 'package)

(setq package-list
      '(avy
        clang-format
        change-inner
        company
        counsel
        dired+
        dired-subtree
        expand-region
        leuven-theme
        magit
        multiple-cursors
        projectile
        speedbar
        sr-speedbar
        swiper
        zenburn-theme
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
