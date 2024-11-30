    ;;; init.el --- Emacs configuration

;;; Commentary:
;; Building a configuration from the ground up with the intent of
;; understanding all the things in it.

;;; License:

;;; Code:

;; Directory definitions
(defvar root-dir (file-name-directory load-file-name)
  "The root of the Emacs configuration directory tree.")
(defvar lisp-dir (expand-file-name "lisp" root-dir)
  "Directory for custom Lisp that defines the configuration.")
;;(setq saves-dir (expand-file-name "saves" user-emacs-directory))

;; load additional lisp files from the lisp-dir directory
(add-to-list 'load-path lisp-dir)

;; Save changes made through the customize interface in their own file
(setq custom-file (expand-file-name "custom.el" root-dir))

;; Measure startup time
(require 'init-benchmarking)
(require 'init-startup)
(require 'init-savefiles)

(require 'init-keybindings)

(provide 'init)
;;; init.el ends here
