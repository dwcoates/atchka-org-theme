;;; atchka-org-theme --- A nice-looking way of presenting Org interactive programs
;;
;;; Commentary:
;;
;; Dodge W. Coates.  Inspired by Alisa Leshchenko, coolest gal in the world.
;;
;;; Code:

(require 'atchka-org)

(deftheme atchka-org "A dark theme for org-mode.")

(defgroup atchka-org-faces nil
  "Open helm."
  :prefix "atchka-org-"
  :group 'faces)

(defface atchka-org-block-lines-face
  `((((background dark))
     (:background "#00688b"
      :foreground "#00688b"
      :height ,atchka--org-block-header-height))
    (((background light))
     '(:background "#00688b"
       :foreground "#00688b"
       :height ,atchka--org-block-header-height)))
  "Face used for source block content"
  :group 'atchka-org-faces)

(defface atchka-org-source-block-face
  '((((background dark)) :color "gray25")
    (((background light)) :color "SlateGray"))
  :group 'atchka-org-faces)

(let* ((c '((class color) (min-colors 89)))
       (bold   doom-enable-bold)
       (italic doom-enable-italic)
       (sans-font (cond ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                        ((x-list-fonts "Verdana") '(:font "Verdana"))
                        ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))
                        (nil (warn "Cannot find a Sans Serif Font."))))
       (org-agenda-font (cond ((x-list-fonts "Liberation Serif") '(:font "Liberation Serif"))
                              (nil (warn "No Agenda Font"))))
       (padding `(:line-width 5))
       (current-line   "#1F1F1F")
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
   'atchka-org
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
   `(org-block                    ((,c (:inherit atchka-org-source-block-face))))
   `(org-block-background         ((,c (:inherit atchka-org-source-block-face))))
   `(org-block-begin-line         ((,c (:inherit atchka-org-block-lines-face))))
   `(org-block-end-line           ((,c (:inherit atchka-org-block-lines-face))))
   ;; Embellishments
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

(defvar atchka-org-babel-languages
  '("asymptote" "C" "clojure" "css" "ditaa" "dot" "emacs-lisp" "gnuplot"
    "haskell" "js" "latex" "ledger" "lisp" "matlab" "mscgen" "ob-doc-ocaml"
    "octave" "oz" "perl" "plantuml" "python" "R" "ruby" "sass"
    "scheme" "ob-doc-screen" "sh" "sql" "sqlite" "ipython" "org" "example"))

;; I don't know why this is still necessasry. I would like to get rid of it.
(setq org-src-block-faces
      (mapcar (lambda (lang) (list lang 'atchka-org-source-block-face))
              atchka-org-babel-languages))


(provide-theme 'atchka-org)

;;; atchka-org-theme ends here
