;;; init-ui.el --- Visual elements for buffers and frames

;;; Commentary:

;;; License:

;;; Code:
(setq-default indent-tabs-mode nil) ; Use spaces instead of tabs
(column-number-mode)                ; Display the column number in modeline
(set-fringe-mode 10)                ; Add space around the fringes
(global-hl-line-mode t)             ; Higlight the current line

;; Display line numbers in text and prog modes
(dolist (mode '(prog-mode-hook
		text-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode t))))

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

;; Display guides at 72, 80, and 100 characters
(global-display-fill-column-indicator-mode t)
(setq-default indicate-buffer-boundaries 'left)

(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(defun dired-default-directory-on-left ()
  "Display `default-directory' in side window on left, hiding details."
  (interactive)
  (let ((buffer (dired-noselect default-directory)))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . fit-window-to-buffer)
              (preserve-size . (t . nil)) ,parameters))))

(provide 'init-ui)
;;; init-ui.el ends here
