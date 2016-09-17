(require 'package)

(setq package-list
      '(avy
        clang-format
        change-inner
        company
        counsel
        dired+
        dired-subtree
        elpy
        expand-region
        haskell-mode
        ivy
        leuven-theme
        magit
        multiple-cursors
        projectile
        speedbar
        sr-speedbar
        swiper
	use-package
        yaml-mode
	zenburn-theme
	))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
     			 ("org" . "http://orgmode.org/elpa/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")))

(package-initialize)

(package-refresh-contents)

(dolist (package package-list)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))
