;;; atchka-org --- A nice-looking way of presenting Org interactive programs
;;
;;; Commentary:
;;
;; Dodge W. Coates.  Inspired by Alisa Leshchenko, coolest gal in the world.
;;
;;; Code:

(require 'org)

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
  '((((background dark)) :background "gray25")
    (((background light)) :background "SlateGray"))
  "`atchka-org' Org source block face"
  :group 'atchka-org-faces)


;; Markup
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
      org-hide-emphasis-markers t
      org-hidden-keywords '(title)
      org-hide-leading-stars t)

;; Source block content
(setq org-src-fontify-natively t     ; syntax highlighting in source blocks
      org-src-tab-acts-natively t)   ; expected tabbing in source block

;; Pretty symbols
(defun atchka-org/pretty-symbols-org-mode-hook ()
  (unless org-pretty-entities (org-toggle-pretty-entities))
  (setq org-pretty-entities-include-sub-superscripts t
        local-abbrev-table org-abbrev-table))
(add-hook 'org-mode-hook 'atchka-org/pretty-symbols-org-mode-hook)

;; Makes source blocks in org look prettier, and generally, org documents should
;; never exceed 80 columns or so. I use M-q (fill-column) constantly to enforce
;; this, which I think looks prettier and neater.
; (add-hook 'window-configuration-change-hook (lambda () }
;                                               (when (eq major-mode 'org-mode) }
;                                                 (set-window-fringes }
;                                                  (selected-window) 0 7)))) }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ORG SOURCE BLOCKS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst atchka--org-block-header-height 1 "Height for org block lines.")

(when (require 'yasnippet nil t)
  (defun yas--show-org-block-lines ()
    "Enlarge block lines when in an Org buffer.
This is used to show hidden blocks in `org-mode' while expanding a snippet."
    (interactive)
    (when (and (boundp 'yas-minor-mode) (equal yas-minor-mode t))
      (let ((s (buffer-substring-no-properties
                (line-beginning-position) (point))))
        (when (member s
                      (apply
                       'append
                       (mapcar
                        (lambda (dir)
                          (let ((dir (concat (file-name-as-directory
                                              (if (symbolp dir) (symbol-value dir) dir))
                                             "org-mode")))
                            (when (f-directory-p dir)
                              (directory-files dir))))
                        yas-snippet-dirs)))
          (org-show-block-lines)))))

  (add-hook 'yas-before-expand-snippet-hook 'yas--show-org-block-lines)
  (add-hook 'yas-after-exit-snippet-hook 'org-hide-block-lines)
  )

(defun org-show-block-lines ()
  "Show the Org-block lines.
This is useful because the atchka theme obfuscates block markup."
  (interactive)
  (set-face-attribute 'org-block-begin-line
                      (selected-frame)
                      :height 100
                      :foreground "black"))

(defun org-hide-block-lines ()
  "Hide the org block lines."
  (interactive)
  (set-face-attribute 'org-block-begin-line nil
                      :height (truncate (* atchka--org-block-header-height 10))
                      :foreground (face-attribute 'org-block-begin-line :background)))

;; Default bindings
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
    (forward-line)))

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
    (forward-line -1)))
(advice-add 'previous-line :before 'org-skip-source-previous-advice)

(defvar protect-faces-priority 9999)

(define-minor-mode protect-faces-mode
  "Make faces immune to distractions from overlays by using more overlays."
  nil nil nil
  (cond (protect-faces-mode
         (unless font-lock-mode
           (user-error "Enable font-lock mode first"))
         (add-hook 'jit-lock-functions #'protect-faces-region 'append t)
         (save-excursion
           (protect-faces-region (point-min) (point-max))))
        (t
         (remove-overlays 1 (point-max)'protect-faces t)
         (jit-lock-unregister #'protect-faces-region))))

(defun protect-faces-region (begin end)
  (interactive "r")
  (remove-overlays begin end 'protect-faces t)
  (goto-char begin)
  (while (< (point) end)
    (let ((face (get-text-property (point) 'face))
          (next (next-single-property-change (point) 'face nil end)))
      (cond
       ((memqr face '(org-block-begin-line
                      org-block-end-line
                      atchka-org-source-block-face))
        (let ((ov (make-overlay (point) next)))
          (overlay-put ov 'priority protect-faces-priority)
          (overlay-put ov 'protect-faces t)
          (overlay-put
           ov 'face 'atchka-org-block-lines-face)))
       (t nil))
      (goto-char next))))

(defface atchka-org-agenda-small-font-face
  '((t :height 85))
  "Temporary buffer-local face")

(defvar atchka-org-agenda-window-width-threshold 80
  "Window width at which org agenda shrinks its font.")

(defun atchka-org--agenda-text-rescale ()
  (if (< (window-width) atchka-org-agenda-window-width-threshold)
      (buffer-face-set 'atchka-org-agenda-small-font-face)
    (buffer-face-set 'default)
      ))

(defun atchka-org-toggle-agenda-text-rescale ()
  (interactive)
  (if (and (or
            (member 'atchka-org--agenda-text-rescale 'window-configuration-change-hook)
            (member 'atchka-org--agenda-text-rescale 'org-agenda-mode-hook))
           (eq (major-mode) 'org-mode))
      (progn
        (add-hook 'org-agenda-mode-hook 'atchka-org--agenda-text-rescale)
        (add-hook 'window-configuration-change-hook 'atchka-org--agenda-text-rescale))
    (remove-hook 'org-agenda-mode-hook 'atchka-org--agenda-text-rescale)
    (remove-hook 'window-configuration-change-hook 'atchka-org--agenda-text-rescale)))

(atchka-org-toggle-agenda-text-rescale)



;; Abbreviations. The ~car~ of the list will be substitited for the ~cdr~.  This is
;; useful because Org is now set up to prettify =\word=, with the corresponding latex
;; symbol, "\alpha" becomes α. Don't want to do this directly because it will
;; interferes with inline latex

(defun atchka-org--create-org-abbrev-table (latex-keywords)
  "Produce an `abbrev-table' for org mode using LATEX-KEYWORDS.
LATEX-KEYWORDS is a list of latex keywords without backslashes
that orgmode also recognizes as the corresponding UTF-8 symbol.
For example, '('alpha' 'beta') will return (('alphaa'
'\\alpha') ('Alphaa' '\\Alpha') ('betaa' '\\beta') ('Betaa'
'\\Beta')), the idea being that one types the symbol name with an
extra character on the end, and abbrev will translate it to the
corresponding latex keyword, which org-mdoe will render as the
corresponding Unicode symbol."
  (apply 'append
         (mapcar
          (lambda (word)
            (mapcar
             (lambda (word)
               (list (concat word (car (last (split-string word "" t))))
                     (concat "\\" word)))
             (list word (capitalize word))))
          latex-keywords)))

(define-abbrev-table 'org-abbrev-table
  (atchka-org--create-org-abbrev-table
   '("alpha" "lambda" "sigma" "rho" "beta" "eta" "delta"
     "epsilon" "zeta" "theta" "iota" "kappa" "mu" "nu"
     "xi" "omicron" "pi" "tau" "upsilon" "phi" "chi" "psi"
     "omega")))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))
  (when (not window-system)
    (custom-set-faces '(default ((t (:background nil)))))))

(provide 'atchka-org)

;;; atchka-org ends here
