;;; init-window_mgmt.el --- Define frame & window size, pos, & functions

;;; Commentary:
;; Set the default frame size and window layout
;;
;; Put each project into its own tab.
;; Use a window layout that approximates:
;;     ___________________________________________________________
;;    |     |                            |                        |
;;    |  *  |                            | *Help*/*Info*/         |
;;    |  t  |                            | *Apropos*              |
;;    |  r  |                            |                        |
;;    |  e  |     Main Window            |                        |
;;    |  e  |                            |                        |
;;    |  m  |                            |                        |
;;    |  a  |                            |                        |
;;    |  c  |____________________________|________________________|
;;    |  s  |                                                     |
;;    |  *  | *shells*                                            |
;;    |_____|_____________________________________________________|
;;
;;; License:

;;; Code:

(setq switch-to-buffer-in-dedicated-window 'pop
      switch-to-buffer-obey-display-actions t)

(defun rjd-add-to-frame-width (additional-width)
  "Add `additional-width' characters to the default frame width"
  (add-to-list 'default-frame-alist
               (cons 'width (+ (frame-width) additional-width))))

;; Side window options
;;     Make left/right side windows occupy the frame's full height
;;     Max number of side windows - left, top, right, bottom
(setq window-sides-vertical t           ; full height left/right side windows
      window-sides-slots '(1 0 2 1))    ; left, top, right, bottom

;; Tab-bar options
;;     Only display tabs if there are more than one tabs to display
;;     Change format of displayed tab-bar information to use tab groups
(setq tab-bar-show 1
      tab-bar-format '(tab-bar-format-tabs-groups
                       tab-bar-separator
                       tab-bar-format-add-tab))
(tab-bar-mode)


;; Project Tabs
;; With inspiration from Mikey Peterson's Mastering Emacs Website
;;     https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(defun mp-buffer-has-project-p (buffer action)
  (with-current-buffer buffer (project-current nil)))

(setq rjd-project-type-shorthands '((Git . "Git")))
(defun rjd-project-type (project)
  (alist-get (car project) rjd-project-type-shorthands "Unk"))

(defun rjd-unique-project-name (buffer alist)
  (with-current-buffer buffer
    (let ((project (project-current)))
      (if project
          (concat (rjd-project-type (cdr project)) " " (project-root project))
        "Misc"))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)
(setq display-buffer-alist
      `(
        ;; Consolidate Help/Info/Apropos into at most two windows to the right.
	;; One caveat, use a second window if opening from a Help/Info/Apropos
        ;; window to make looking at an index or contents easy in side-by-side
        ;; windows
	(,(rx (| "*Help*" "*info*" "*Apropos*"))
	 (display-buffer-reuse-mode-window
          display-buffer-in-direction)
	 (direction . right)
	 (mode . (help-mode Info-mode apropos-mode))
	 (inhibit-same-window . t))

        ;; Put shells in a bottom "side" window
        ("^\\*\\(.*-\\)?e?shell\\*$"
         display-buffer-in-side-window
         (side . bottom)
         (slot . 0)
         (window-parameters . ((no-other-window . t)
                               (no-delete-other-windows . t))))

        ;; Group projects and give each its own tab
        (mp-buffer-has-project-p
         (display-buffer-in-tab
          display-buffer-reuse-window)
         (tab-name . rjd-unique-project-name)
         (tab-group . "Projects:"))
        ))

(provide 'init-window_mgmt)
;;; init-window_mgmt.el ends here
