;;; lang-proto.el --- Options for protobuf files

;;; Code::

(add-to-list 'treesit-language-source-alist
             '(proto "https://github.com/mitchellh/tree-sitter-proto"))

(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-ts-mode))


(provide 'lang-proto)
;;; lang-proto.el ends here
