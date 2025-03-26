;;; init-project.el --- Project management and interfaces to it

;;; Commentary:

;;; License:

;;; Code

;; Imenu
(setq imenu-auto-rescan t)

;; Treemacs buffer
(setq treemacs-tag-follow-mode t
      treemacs-width 35
      treemacs-text-scale nil
      treemacs-tag-follow-cleanup nil)

(with-eval-after-load 'treemacs
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable (treemacs-git-commit-diff-mode t))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t) (treemacs-git-mode 'deferred))
    (`(t . _) (treemacs-git-mode 'simple)))
  (treemacs-hide-gitignored-files-mode nil))

(add-hook 'dired-mode-hook 'treemacs-icons-dired-enable-once)
(rjd-add-to-frame-width treemacs-width)
(treemacs-start-on-boot)

(provide 'init-project)
;;; init-project.el
