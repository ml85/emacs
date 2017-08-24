(require 'package)

(setq package-list
      '(ag
        avy
        ;;clang-format
        change-inner
        ;;cmake-ide
        ;;cmake-mode
        company
        company-irony
        company-irony-c-headers
        counsel
        dired+
        dired-subtree
        elpy
        evil
        exec-path-from-shell
        expand-region
        flycheck
        ;;haskell-mode
        irony
        ivy
        js2-mode
        js2-refactor
        key-chord
        magit
        multiple-cursors
        neotree
        projectile
        ;;speedbar
        ;;sr-speedbar
        swiper
        use-package
        ;;yaml-mode
        yasnippet
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
