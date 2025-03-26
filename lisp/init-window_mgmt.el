;;; init-window_mgmt.el --- Define frame & window size, pos, & functions

;;; Commentary:
;; Set the default frame size

;;; License:

;;; Code:

(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

(defun rjd-add-to-frame-width (additional-width)
  "Add `additional-width' characters to the default frame width"
  (add-to-list 'default-frame-alist
               (cons 'width (+ (frame-width) additional-width))))

;; Max number of side windows - left, top, right, bottom
(setq window-sides-slots '(1 0 1 1))


;; Consolidate Help/Info/Apropos into at most two windows to the right.
;; One caveat, use a second window if opening from a Help/Info/Apropos window
;; to make looking at an index or contents easy in side-by-side windows
(add-to-list 'display-buffer-alist
             `(,(rx (| "*Help*" "*info*" "*Apropos*"))
               (display-buffer-reuse-mode-window
                display-buffer-in-direction)
               (direction . right)
               (mode . (help-mode Info-mode apropos-mode))
               (inhibit-same-window . t)))
               
;; (add-to-list 'display-buffer-alist
;;              `(,(rx (| "*compilation*" "*grep*"))
;;                display-buffer-in-side-window
;;                (side . right)
;;                (slot . 0)
;;                (window-parameters . ((no-delete-other-windows . t)))
;;                (window-width . 80)))

;; (setq tab-bar-format '(tab-bar-format-history
;;                        tab-bar-format-tabs-groups
;;                        tab-bar-separator
;;                        tab-bar-format-add-tab))

;; Project Tabs
;; From Mikey Peterson's Mastering Emacs Website
;;     https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun mp-buffer-has-project-p (buffer action)
  (with-current-buffer buffer (project-current nil)))

(defun rjd-unique-project-name (buffer alist)
  (when-let ((project (project-current nil)))
    (project-root (project))))
;;  (project-root (project-current nil)))

(add-to-list 'display-buffer-alist
             '(mp-buffer-has-project-p
               (display-buffer-in-tab
                display-buffer-reuse-window)
               (tab-name . rjd-unique-project-name)))

;; (defun mp-tab-group-name (buffer alist)
;;   (with-current-buffer buffer
;; ;;    (concat "🗃 " (or (cdr (project-current nil)) "🛡 Ungrouped"))))
;;     (concat "(P) " (or (project-root (project-current nil)) "(U) Ungrouped"))))

;; (defun mp-tab-tab-name (buffer alist)
;;   (with-current-buffer buffer
;;     (buffer-name)))

;; (add-to-list 'display-buffer-alist
;;              '(mp-buffer-has-project-p
;;                (display-buffer-in-tab display-buffer-reuse-window)
;; 	       (tab-name . mp-tab-tab-name)
;; 	       (tab-group . mp-tab-group-name)
;; 	       ))

;; ;; (defun tab-bar-tabs-set (tabs &optional frame)
;; ;;   "Set a list of TABS on the FRAME."
;; ;;   (set-frame-parameter frame 'tabs (seq-sort-by (lambda (el)
;; ;;                                                   (alist-get 'group el nil))
;; ;;                                                 #'string-lessp
;; ;;                                                 tabs)))
;; ;; (defun mp-reload-tab-bars (&optional dummy)
;; ;;   "Reload the tab bars... because they're buggy."
;; ;;   (interactive)
;; ;;   (tab-bar-tabs-set (frame-parameter nil 'tabs)))

;; ;; (add-hook 'kill-buffer-hook #'mp-reload-tab-bars)
;; ;; (add-hook 'window-selection-change-functions #'mp-reload-tab-bars)

(tab-bar-mode)

(provide 'init-window_mgmt)
;;; init-window_mgmt.el ends here
