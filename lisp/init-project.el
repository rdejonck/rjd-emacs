;;; init-project.el --- Project management

;;; Commentary:

;;; License:

;;; Code:
(require 'project)

(defun rjd-vc-dir-show-all-files ()
  "Display all files in a VC project in the vc-dir window"
  (interactive)
  (with-current-buffer "*vc-dir*"
    (mapc 'vc-dir-show-fileentry (project-files (project-current)))))

;; Make manual buffer switching obey our rules
(setq switch-to-buffer-obey-display-actions t)

(defvar sidebar-window-parameters
  '(window-parameters . (;(no-other-window . t)
                         (no-delete-other-windows . t))))
(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

(defvar-local my-project-font-remap-cookie nil
  "The current face remap of `my-project-set-font'.")
(defface my-project-default
  '((t :height 0.8))
  "Face for the project buffers.")
(defun my-project-set-font ()
  (setq-local my-project-font-remap-cookie
              (face-remap-add-relative 'default 'my-project-default)))

(add-to-list 'display-buffer-alist
             `(,(rx (| "*vc-dir*"))
               display-buffer-in-side-window
               (side . left)
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))
               (window-width . fit-window-to-buffer)))

  
(defun rjd-directory-tree-left-window ()
  "Display the current project's directory tree in a left side window"
  (interactive)
  (if (vc-root-dir)
      ((vc-dir-root)
       (my-project-set-font))
    (let ((buffer (dired-noselect default-directory)))
      (with-current-buffer buffer
        (dired-hide-details-mode t)
        (rename-buffer "*Dired Files*"))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . fit-window-to-buffer)
              (preserve-size . (t . nil))
              (mode-line-format . (" " "%b"))
              ,sidebar-window-parameters))))

         
;;          buffer '((side
;;   (unless (vc-root-dir)
;;     (let ((buffer (dired-noselect default-directory)))
;;       (rename-buffer "*Project Files
;;   (let ((buffer (if (eq (vc-root-dir) nil)
;;                     (dired-noselect default-directory)
;;                   (vc-dir-root)))))
;;                   (dired-noselect (vc-root-dir)))))
;;     (with-current-buffer buffer
;;       (dired-hide-details-mode t)
;;       (setq dired-free-space nil)
;; ;      (setq dired-listing-switches "-lRF")
;;      (rename-buffer "*RJD-Dired-Side*"))

(provide 'init-project)
;;; init-project.el ends here
