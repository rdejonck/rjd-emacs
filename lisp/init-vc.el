;;; init-vc.el: Initialize and configure version control software

;;; Commentary:
;; Setup Magit and other Version Control packages

;;; License:

;;; Code:

;; Setup the following key bindings:
;;     C-x g  magit-status
;;     C-c g  magit-dispatch
;;     C-c f  magin-file-dispatch
(setq magit-define-global-key-bindings 'recommended)

;; Highlight uncommitted changes in the window fringe
;; Key bindings:
;;     C-x v =  diff-hl-goto-hunk
;;     C-x v n  diff-hl-revert-hunk
;;     C-x v [  diff-hl-previous-hunk
;;     C-x v ]  diff-hl-next-hunk
;;     C-x v *  diff-hl-show-hunk
;;     C-x v S  diff-hl-stage-current-hunk
(global-diff-hl-mode)
(diff-hl-flydiff-mode)
;; Show diff marks in the margin instead of the fringe in the terminal
(unless (window-system) (diff-hl-margin-mode))
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)

(provide 'init-vc)
;;; init-vc.el
