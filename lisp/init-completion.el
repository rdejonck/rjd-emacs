;;; init-completion.el --- Setup completion in minibuffers and elsewhere

;;; Commentary:
;; Use:
;;     Vertico:
;;     Marginalia:
;;     Orderless:
;;     Embark:
;;     Consult:
;;     Corfu:

;;; License:

;;; Code:


;; Vertico
;(setq vertico-cycle t
;      vertico-resize nil)
(vertico-mode)


;; Marginalia
(add-hook 'marginalia-mode-hook 'nerd-icons-completion-marginalia-setup)
(marginalia-mode)


;; Orderless
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides nil)


;; Consult
(add-hook 'completion-list-mode 'consult-preview-at-point-mode)
(advice-add #'register-preview :override #'consult-register-window)
(setq register-preview-delay 0.5)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)


;; Embark
(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'embark-collect-mode 'consult-preview-at-point-mode)


;; Corfu
(global-corfu-mode t)
(setq tab-always-indent 'complete
      corfu-preview-current nil
      corfu-min-width 20
      corfu-popupinfo-delay '(1.25 . 0.5)
      corfu-cycle t
      corfu-auto t
      corfu-quit-no-match 'separator
      text-mode-ispell-word-completion nil
      read-extended-command-predicate #'command-completion-default-include-p)
(corfu-popupinfo-mode 1)
(with-eval-after-load 'savehist
  (corfu-history-mode 1)
  (add-to-list 'savehist-additional-variables 'vorfu-history))
;; Unbind RET from Corfu so RET we can use RET to start a newline
(keymap-unset corfu-map "RET")
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)


;; Capf
(add-to-list 'completion-at-point-functions #'cape-abbrev)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dict)


(add-hook 'dired-mode-hook 'nerd-icons-dired-mode) ; Move somewhere appropriate

(provide 'init-completion)
;;; init-completion.el ends here
