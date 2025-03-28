;;; init-theme.el --- Theme appearance for X display

;;; Commentary:

;;; License:

;;; Code:

;; Use the built-in modus-themes
(require-theme 'modus-themes)

(defun rjd-configure-modus-themes-v3 ()
  "Configure modus-theme 3.0.0"
  (interactive)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        ;; modus-themes-mixed-fonts nil
        modus-themes-subtle-line-numbers t
        modus-themes-intense-mouseovers t
        ;; modus-themes-deuteranopia t
        modus-themes-tabs-accented t
        ;; modus-themes-variable-pitch-ui nil
        ;; modus-themes-inhibit-reload t ; only applies to 'customize-set-variable' and related
        modus-themes-fringes 'subtle ; {nil, 'subtle, 'intense}

        ;; Options for `modus-themes-lang-checkers' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `straight-underline', `text-also', `background',
        ;; `intense' OR `faint'.
        ;; modus-themes-lang-checkers nil
        
        ;; Options for `modus-themes-mode-line' are either nil, or a list
        ;; that can combine any of `3d' OR `moody', `borderless',
        ;; `accented', a natural number for extra padding (or a cons cell
        ;; of padding and NATNUM), and a floating point for the height of
        ;; the text relative to the base font size (or a cons cell of
        ;; height and FLOAT)
        modus-themes-mode-line '(3d accented borderless (padding . 1) (height . 1.0))
        ;; Same as above:
        ;; modus-themes-mode-line '(3d accented borderless 1 1.0)

        ;; Options for `modus-themes-markup' are either nil, or a list
        ;; that can combine any of `bold', `italic', `background',
        ;; `intense'.
        ;; modus-themes-markup '(background italic)

        ;; Options for `modus-themes-syntax' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
        modus-themes-syntax '(yellow-comments green-strings alt-syntax)

        ;; Options for `modus-themes-hl-line' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `accented', `underline', `intense'
        modus-themes-hl-line '(accented intense)

        ;; Options for `modus-themes-paren-match' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `bold', `intense', `underline'
        ;; modus-themes-paren-match '(bold intense)

        ;; Options for `modus-themes-links' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
        ;; `bold', `italic', `background'
        ;; modus-themes-links '(neutral-underline background)

        ;; Options for `modus-themes-box-buttons' are either nil (the
        ;; default), or a list that can combine any of `flat', `accented',
        ;; `faint', `variable-pitch', `underline', `all-buttons', the
        ;; symbol of any font weight as listed in `modus-themes-weights',
        ;; and a floating point number (e.g. 0.9) for the height of the
        ;; button's text.
        ;; modus-themes-box-buttons '(variable-pitch flat faint 0.9)

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `background', `bold', `gray', `intense', `italic'
        ;; modus-themes-prompts '(intense bold)

        ;; The `modus-themes-completions' is an alist that reads three
        ;; keys: `matches', `selection', `popup'.  Each accepts a nil
        ;; value (or empty list) or a list of properties that can include
        ;; any of the following (for WEIGHT read further below):
        ;;
        ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
        ;; `selection' - `accented', `intense', `underline', `italic', `text-also' WEIGHT
        ;; `popup' - same as `selected'
        ;; `t' - applies to any key not explicitly referenced (check docs)
        ;;
        ;; WEIGHT is a symbol such as `semibold', `light', or anything
        ;; covered in `modus-themes-weights'.  Bold is used in the absence
        ;; of an explicit WEIGHT.
        ;; modus-themes-completions '((matches . (extrabold))
        ;;                           (selection . (semibold accented))
        ;;                           (popup . (accented intense)))

        ;; modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

        ;; Options for `modus-themes-region' are either nil (the default),
        ;; or a list of properties that may include any of those symbols:
        ;; `no-extend', `bg-only', `accented'
        modus-themes-region '(bg-only no-extend)

        ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
        ;; modus-themes-diffs 'desaturated

        ;; modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

        ;; modus-themes-org-agenda ; this is an alist: read the manual or its doc string
        ;; '((header-block . (variable-pitch 1.3))
        ;;  (header-date . (grayscale workaholic bold-today 1.1))
        ;;  (event . (accented varied))
        ;;  (scheduled . uniform)
        ;;  (habit . traffic-light))

        ;; modus-themes-headings ; this is an alist: read the manual or its doc string
        ;; '((1 . (overline background variable-pitch 1.3))
        ;;  (2 . (rainbow overline 1.1))
        ;;  (t . (semibold))))
        )
  (load-theme 'modus-vivendi))

(defun rjd-configure-modus-themes-v4 ()
  "Configure modus-theme 4.x"
  (interactive)
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-custom-auto-reload t
        modus-themes-disable-other-themes t

        modus-themes-common-palette-overrides
        '(
          ;; Remove the modeline border. TODO: make it 3D
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; Change up the comment colors
          (comment green-faint)
          (docmarkup green-faint)

          ;; Make TODO and DONE more intense
          (prose-done green-intense)
          (prose-todo red-intense)

          ;; Make code blocks in Org and other modes more colorful
          (bg-prose-block-contents bg-blue-nuanced)
          (bg-prose-block-delimeter bg-lavender)
          (fg-prose-block-delimeter fg-main)
          (bg-prose-code bg-green-nuanced)
          (fg-prose-code green-cooler)
          (bg-prose-verbatim bg-magenta-nuanced)
          (fg-prose-verbatim magenta-warmer)
          (bg-prose-macro bg-blue-nuanced)
          (fg-prose-macro magenta-cooler)

          ;; Make the region preserve text colors, plus other styles
          (bg-region bg-cyan-nuanced)
          (fg-region unspecified)
	  ))
    (load-theme 'modus-vivendi-tinted))

(defun rjd-configure-ef-themes ()
  "Configure ef-themes"

  ;; Give the modeline a 3-D look
  (custom-set-faces
   '(mode-line ((t :box (:line-width 1 :style released-button))))
   '(mode-line-inactive ((t :box (:line-width 1 :style released-button)))))

  ;; Darken the ef-owl theme's backgrounds
  (setq ef-owl-palette-overrides
        '((bg-main "#141617")
          (bg-completion "#1a2432")))

  ;; The defaults look good on some monitors and washed out on others.
  ;; Leaving them here, commented out, and checking the monitor color rendering
  ;; (setq ef-elea-dark-palette-overrides
  ;;       '((bg-main "#020504")))
  ;; (setq ef-maris-dark-palette-overrides
  ;;       '((bg-main "#030c1b")))

  (ef-themes-select 'ef-owl)

  (setq 
        ;; Choose a set of themes to rotate through with `ef-themes-rotate'
        ;; See all choices in `ef-themes-collection'
        ;;   Light choices: `ef-themes-light-themes'
        ;;   Dark choices:  `ef-themes-dark-themes'
        rjd-ef-themes-dark '(ef-dark  ef-deuteranopia-dark  ef-elea-dark
                             ef-maris-dark  ef-night  ef-owl  ef-winter)
        ef-themes-to-rotate ef-themes-collection

        ;; Toggle between a light and dark theme with `ef-themes-toggle'
        ef-themes-to-toggle '(ef-owl ef-frost)
        )

  )

(mapc #'disable-theme custom-enabled-themes)
;; Use ef-themes if they are available, otherwise fallback to modus-theme
;; Do nothing for emacs older than 28
(cond ((featurep 'ef-themes) (rjd-configure-ef-themes))
      ((not (version< emacs-version "30")) (rjd-configure-modus-themes-v4))
      ((not (version< emacs-version "28")) (rjd-configure-modus-themes-v3)))

(provide 'init-theme)
;;; init-theme.el ends here
