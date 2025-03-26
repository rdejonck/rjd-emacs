;;; init-project.el --- Project management and interfaces to it

;;; Commentary:

;;; License:

;;; Code

;; Treemacs buffer
(setq treemacs-tag-follow-mode t
      treemacs-filewatch-mode t
      treemacs-fringe-indicator-mode 'always
      treemacs-hide-gitignored-files-mode nil
      treemacs-width 35)
;(when treemacs-python-executable (treemacs-git-commit-diff-mode))
;(pcase (cons (not (null (executable-find "git")))
;             (not (null treemacs-python-executable)))
;  (`(t . t) (treemacs-git-mode 'deferred))
;  (`(t . t) (treemacs-git-mode 'simple)))

(add-hook 'dired-mode-hook 'treemacs-icons-dired-enable-once)
(rjd-add-to-frame-width treemacs-width)
(treemacs-start-on-boot)

(provide 'init-project)
;;; init-project.el
