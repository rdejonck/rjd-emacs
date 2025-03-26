;;; init-packages.el --- Setup package repositories and load packages

;;; Commentary:
;; This file contains a consolidated list of all the packages to load.
;; The concept is borrowed from prelude emacs

;;; License:

;;; Code:
(require 'cl-lib)
(require 'package)

(defvar my-packages
  '(
    diff-hl
    magit
    treemacs
    treemacs-magit          ; Ensure after treemacs and magit
    treemacs-icons-dired
    )
  "A list of packages to ensure are installed at launch.")

;; ;; Package archives to search. Append trusted archives
;; ;; Note: nongnu is included by default in emacs 29+
;; (when (version< emacs-version "28")
;;   (add-to-list 'package-archives
;;                '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))
;; Append the melpa stable and melpa archives
(add-to-list 'package-archives
             '("stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages") t)
;; Install into separate package dirs for each Emacs version
(setq package-user-dir
      (expand-file-name
       (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
       root-dir))

;; Initialize the package system
(package-initialize)

(defun my-packages-installed-p ()
  "Check if all packages in `my-packages' are installed."
  (cl-every #'package-installed-p my-packages))

(defun my-require-package (package)
  "Install PACKAGE unless it is already installed."
  (unless (memq package my-packages)
    (add-to-list 'my-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun my-require-packages (packages)
  "Ensure PACKAGES are instaled.
Missing packages are installed automatically."
  (mapc #'my-require-package packages))

(defun my-install-packages ()
  "Install all packages listed in `my-packages'."
  (unless (my-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing the package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (my-require-packages my-packages)))

;; run package installation
(my-install-packages)

(provide 'init-packages)
;;; init-packages.el
