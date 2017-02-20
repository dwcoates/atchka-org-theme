;;; atchka-theme --- A nice-looking way of presenting Org interactive programs
;;
;;; Commentary:
;;
;; Dodge W. Coates.  Inspired by Alisa Leshchenko, coolest gal in the world.
;;
;;; Code:

(deftheme atchka "A dark theme.")

(defconst atchka--org-block-header-height 0.1 "Height for org block lines.")

((let* ((c '((class color) (min-colors 89)))
        (bold   doom-enable-bold)
        (italic doom-enable-italic)
        (sans-font (cond ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                         ((x-list-fonts "Verdana") '(:font "Verdana"))
                         ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                         (nil (warn "Cannot find a Sans Serif Font."))))
        (org-agenda-font (cond ((x-list-fonts "Liberation Serif") '(:font "Liberation Serif"))
                               (nil (warn "No Agenda Font"))))
        (padding `(:line-width 5))
        ;; Colors
        (cyan           "#66D9EF")
        (orange         "#FD971F")
        (grey           "#C0C5CF")
        (grey-1         "#525254")
        (green          "#B6E63E")
        (violet         "#9C91E4")
        (yellow         "#E2C770")
        (magenta        "#F92672")
        (grey-.5        "#828284"))
   (custom-theme-set-faces
    'atchka
    `(org-tag                      ((,c (:foreground "#E2C770" :bold nil))))
    `(org-hide                     ((,c (:foreground "#1D1F20"))))
    `(org-table                    ((,c (:foreground ,cyan))))
    `(org-quote                    ((,c (:slant italic :foreground ,grey :background ,current-line))))
    `(org-document-info            ((,c (:foreground ,orange))))
    `(org-document-info-keyword    ((,c (:foreground ,grey-1))))
    `(org-meta-line                ((,c (:foreground "#cd3278" :box t))))
    `(org-archived                 ((,c (:foreground ,grey-.5))))
    `(org-document-title           ((,c (:inherit org-level-1 :height 1.5 :underline nil :box ,padding :foreground ,cyan))))
    ;; Org Source Code
    `(org-block                    ((,c (:inherit 'shadow ) :background "gray30" :distant-foreground "white")))
    `(org-block-background         ((,c (:background "gray25"))))
    `(org-block-begin-line         ((,c (:background "SkyBlue4" :foreground "SkyBlue4"  :height ,atchka--org-block-header-height))))
    `(org-block-end-line           ((,c (:inherit org-block-begin-line))))
    `(org-code                     ((,c (:foreground "#cd5c5c"))))
    `(org-verbatim                 ((,c (:foreground ,green))))
    `(org-formula                  ((,c (:foreground ,cyan))))
    ;; Headers
    `(org-level-2                  ((,c (,@sans-font :height 1.07 :foreground "Peru"))))
    `(org-level-3                  ((,c (,@sans-font :foreground "RosyBrown"))))
    `(org-level-4                  ((,c (:inherit 'outline-4))))
    `(org-level-6                  ((,c (:inherit 'outline-7))))
    `(org-level-7                  ((,c (:foreground "yellow green"))))
    `(org-level-1                  ((,c (,@sans-font :height 1.18 :bold t :foreground "DarkKhaki"))))
    ;; Agenda
    `(org-scheduled                ((,c (:foreground "yellow3"))))
    `(org-scheduled-today          ((,c (:foreground "dark orange") :weight bold)))
    `(org-agenda-date              ((,c (:inherit org-agenda-structure :height 1.10))))
    `(org-agenda-date-today        ((,c (:inherit org-agenda-date :bold black :underline t))))
    `(org-agenda-date-weekend      ((,c (:inherit org-agenda-date :italic yes :height .95))))
    `(org-upcoming-deadline        ((,c (:foreground "dark gray") :weight bold)))
    `(org-agenda-structure         ((,c (:inherit default ,@org-agenda-font :height 1.10 :underline nil))))
    `(org-date                     ((,c (:foreground ,violet))))
    ;; Tasks
    `(org-todo                     ((,c (:foreground ,yellow :bold inherit))))
    `(org-done                     ((,c (:foreground ,green :bold inherit))))
    ;; Misc
    `(org-list-dt                  ((,c (:foreground ,cyan))))
    `(org-footnote                 ((,c (:foreground ,orange))))
    `(org-link                     ((,c (:underline t :foreground ,cyan :bold inherit))))
    `(org-headline-done            ((,c (:foreground ,grey-.5 :strike-through t :bold nil))))
    `(org-special-keyword          ((,c (:foreground ,magenta))))
    `(org-checkbox-statistics-todo ((,c (:inherit org-todo))))
    `(org-checkbox-statistics-done ((,c (:inherit org-done))))
    )
   )
 )

(defun yas-show-org-block-lines ()
  "Enlarge block lines when in an Org buffer.
This is used to show hidden blocks in `org-mode' while expanding a snippet."
  (interactive)
  (when (and (boundp 'yas-minor-mode) (equal yas-minor-mode t))
    (let ((s (buffer-substring-no-properties (line-beginning-position)
                                             (point))))
      (when
        (member s (apply
                   'append
                   (mapcar
                    (lambda (dir)
                      (let ((dir
                             (concat (file-name-as-directory
                                      (if (symbolp dir) (symbol-value dir) dir))
                                     "org-mode")))
                      (when (f-directory-p dir)
                          (directory-files dir))))
                    yas-snippet-dirs)))
        (org-show-block-lines)
        ))))

(when (require 'yasnippet nil t)
  (add-hook 'yas-before-expand-snippet-hook 'yas-show-org-block-lines)
  (add-hook 'yas-after-exit-snippet-hook 'org-hide-block-lines))

(defun org-show-block-lines ()
  "Show the Org-block lines.
This is useful because the atchka theme obfuscates block markup."
  (interactive)
  (set-face-attribute 'org-block-begin-line
                      (selected-frame)
                      :height 100
                      :foreground "black")
  )

(defun org-hide-block-lines ()
  "Hide the org block lines."
  (interactive)
  (set-face-attribute 'org-block-begin-line nil
                      :height (truncate (* atchka--org-block-header-height 10))
                      :foreground (face-attribute 'org-block-begin-line :background)))

(global-set-key (kbd "C-c C-v C-;") 'org-show-block-lines)
(global-set-key (kbd "C-c C-v C-:") 'org-hide-block-lines)

(defun org-skip-source-next-advice ()
  "Advice for the `next-line' function.
Please `next-line' past org-block headers'"
  (interactive)
  (when (and (eq major-mode 'org-mode)
             (save-excursion
               (forward-line)
               (call-interactively 'beginning-of-line)
               (or
                (re-search-forward "#\\+begin_src[ ]+?"
                                   (line-end-position) t)
                (re-search-forward "#\\+end_src[ ]*?"
                                   (line-end-position) t))))
    (forward-line))
  )

;(advice-remove 'next-line 'org-skip-source-next-advice)
(advice-add 'next-line :before 'org-skip-source-next-advice)

(defun org-skip-source-previous-advice ()
  "Advice for the `previous-line' function.
Please `previous-line' past org-block headers'"
  (interactive)
  (when (and
         (eq major-mode 'org-mode)
         (save-excursion
          (forward-line -1)
          (call-interactively 'beginning-of-line)
          (or
           (re-search-forward "#\\+begin_src[ ]+?"
                              (line-end-position) t)
           (re-search-forward "#\\+end_src[ ]*?"
                              (line-end-position) t))))
    (forward-line -1))
  )

;(advice-remove 'previous-line 'org-skip-source-previous-advice)
(advice-add 'previous-line :before 'org-skip-source-previous-advice)


;; Makes source blocks in org look prettier, and generally, org documents should
;; never exceed 80 columns or so. I use M-q (fill-column) constantly to enforce
;; this, which I think looks prettier and neater.
(add-hook 'window-configuration-change-hook (lambda ()
                                              (when (eq major-mode 'org-mode)
                                               (set-window-fringes
                                                (selected-window) 30 34))))

;; This hides the asterisks in org headers, so they look like they are indented
(setq org-hide-leading-stars t)

;; I don't know why this is still necessasry. I would like to get rid of it.
(setq org-src-block-faces
      '(("python" (:background "gray25"))
        ("ipython" (:background "gray25"))
        ("emacs-lisp" (:background "gray25"))
        ("R" (:background "gray25"))
        ("org" (:background "gray25"))
        ("example" (:background "gray25"))
        ("latex" (:background "gray25"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))


(provide-theme 'atchka-theme)

;;; atchka-theme ends here
