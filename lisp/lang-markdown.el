;;; lang-markdown.el --- Customizations for markdown editing modes

;;; Code:
(require 'markdown-ts-mode)

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-ts-mode))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
