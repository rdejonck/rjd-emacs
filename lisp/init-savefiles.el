;;; init-savefiles.el --- Locations for automatically saved files

;;; Commentary:
;; set saves-dir to control where automatically saved files are stored

;;; Code:

(defvar saves-dir (expand-file-name "saves" user-emacs-directory)
  "Directory to store all the automatically generated save / history files.")
(unless (file-exists-p saves-dir)
  (make-directory saves-dir))

;; Consolidate backup files (ie '...~' files)
(setq backup-directory-alist
      `(("." . ,saves-dir)))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 0
      version-control t
      vc-make-backup-files t)

;; Autosave files (ie '#...#' temp files
(setq auto-save-no-message nil ; Display 'Autosaving...' message

      ;; Consolidate auto-save files into saves-dir
      auto-save-file-name-transforms
      `((".*" ,(file-name-as-directory saves-dir) t)))
;; Delete auto-save files when killing the buffer
(setf kill-buffer-delete-auto-save-files t)


;; Recent Files List
(setq recentf-save-file (expand-file-name "recentf" saves-dir)
;;      recentf-auto-cleanup 'never ; Uncomment if this causes some unknown issues
      recentf-max-saved-items 500
      recentf-max-menu-items 25)
(recentf-mode t)

;; Save the minibuffer history between sessions
(setq savehist-file (expand-file-name "savehist" saves-dir))
(savehist-mode t)

;; Remember your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" saves-dir))
(save-place-mode t)

(provide 'init-savefiles)
;;; init-savefiles.el ends here
