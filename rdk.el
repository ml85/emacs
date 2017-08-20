;;; rdk.el --- Emacs extension for working with RDK projects -*- lexical-binding: t -*-

;; Copyright © 2017 Mikael Larsson

;; Author: Mikael Larsson
;; URL:
;; Package-Version: 20170721.001
;; Keywords: rdk
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1") (projectile "0.15.0-cvs"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This library...

;;; Code:

(require 'projectile)

(defcustom rdk-sdk-installation-dir "/opt"
  "The sdk installation directory."
  :group 'rdk
  :type 'string)

(defvar rdk-current-sdk nil
  "The current sdk for RDK projects")

(defvar rdk-current-build-type "Debug"
  "The current build type for RDK projects")

(defun rdk--check-if-rdk-project()
  "Checks whether the current project is an RDK project."
  (when (projectile-project-p)
    (file-exists-p (concat (projectile-project-root) ".rdk"))))

(defun rdk--get-installed-os-versions()
  "Returns a list with all installed application os versions"
  (directory-files rdk-sdk-installation-dir nil "rcsos*"))

(defun rdk--get-installed-sdks-for-os-version(os)
  "Returns a list with sdk directories for the given base-dir. The base-dir is appended."
  (mapcar (lambda (sdk) (concat  os "_" sdk))
          (directory-files (concat rdk-sdk-installation-dir "/" os)
                           nil
                           "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun rdk--get-installed-sdks()
  "Returns a list with all installed sdks"
  (let (value)
    (dolist (os (rdk--get-installed-os-versions) value)
      (setq value (append
                   (rdk--get-installed-sdks-for-os-version os)
                   value)))))

(defun rdk-set-current-sdk()
  "Set which RCSOS sdk that shall be used"
  (interactive)
  (let ((arg (completing-read "Select sdk: " (rdk--get-installed-sdks))))
    (setq rdk-current-sdk arg)))

(defun rdk-set-current-build-type()
  "Set which build type that shall be used"
  (interactive)
  (let ((arg (completing-read "Select build type: " '("Debug" "Release"))))
    (setq rdk-current-build-type arg)))
