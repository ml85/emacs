(require 'package)

(setq package-list
      '(ag
        avy
        clang-format
        change-inner
        cmake-ide
        company
        counsel
        dired+
        dired-subtree
        elpy
        expand-region
        flycheck
        haskell-mode
        ivy
        js2-mode
        js2-refactor
        leuven-theme
        magit
        monokai-theme
        multiple-cursors
        neotree
        projectile
        solarized-theme
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
