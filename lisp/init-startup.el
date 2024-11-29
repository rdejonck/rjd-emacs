;;; init-startup.el --- Control startup behavior and basic global preferences

;;; Commentary:

;;; Code:

;; Manage garbage collection
;; Reduce the frequency of garbage
;; Use 128MB during startup and 20MB during normal operation
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
	  (lambda () (setq gc-cons-threshold (* 20 1024 1024))))

(setq visible-bell t)               ; flash instead of an audible bell
(setq inhibit-startup-screen t)     ; Do not display the initial splash screen
(tool-bar-mode -1)                  ; No toolbar
;;(menu-bar-mode -1)                ; No menu bar
;;(scroll-bar-mode -1)              ; No scrollbar

;; enable y/n answers instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)
(when (boundp 'use-short-answers)
  (setq use-short-answers t))

(provide 'init-startup)
;;; init-startup.el ends here
