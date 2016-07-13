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
        ivy
        leuven-theme
        magit
        multiple-cursors
        projectile
        speedbar
        sr-speedbar
        swiper
	use-package
        zenburn-theme
	))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
     			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(package-refresh-contents)

(dolist (package package-list)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))
