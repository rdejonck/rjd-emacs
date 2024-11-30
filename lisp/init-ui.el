;;; init-ui.el --- Visual elements for buffers and frames

;;; Commentary:

;;; License:

;;; Code:
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(column-number-mode)                ; Display the column number in modeline
(set-fringe-mode 10)                ; Add space around the fringes
(global-hl-line-mode t)             ; Higlight the current line

;; Display line numbers in most modes
;; Modes without line numbers are explicitly listed by exception
(global-display-line-numbers-mode t)
(dolist (mode '(term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Truncate long lines in programming modes
(defun prog-mode-fill-setup ()
       (set-fill-column 80)
       (setq truncate-lines t
	     auto-hscroll-mode 'current-line)) ; only scroll the current line
(add-hook 'prog-mode-hook 'prog-mode-fill-setup)

;; Use visual line mode in text modes
(defun text-mode-fill-setup ()
  (set-fill-column 72)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (turn-on-visual-line-mode))
(add-hook 'text-mode-hook 'text-mode-fill-setup)

(provide 'init-ui)
;;; init-ui.el ends here
