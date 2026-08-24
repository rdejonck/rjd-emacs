;;; init-treesit.el --- Enable tree-sitter and all the ts modes

;;; Commentary:

;;; License:

;;; Code:
(setopt treesit-auto-install-grammar t)
(setopt treesit-enabled-modes t)

(defun my/install-all-grammars ()
  "Install as many grammars as possible now, instead of on demand later."
  ;; Useful if you work offline and won't be able to install on demand
  (interactive)
  (dolist (mode treesit-major-mode-remap-alist)
    (with-temp-buffer (funcall (cdr mode)))))

(defun mp/install-all-grammars ()
  "Install as many grammars as possible now, instead of on demand later."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car treesit-language-source-alist)))


(provide 'init-treesit)
;;; init-treesit.el ends here
