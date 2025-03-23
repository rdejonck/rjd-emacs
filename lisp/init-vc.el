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

(provide 'init-vc)
;;; init-vc.el
