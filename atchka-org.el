;;; atchka-org --- A nice-looking way of presenting Org interactive programs
;;
;;; Commentary:
;;
;; Dodge W. Coates.  Inspired by Alisa Leshchenko, coolest gal in the world.
;;
;;; Code:

(require 'org)

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
(add-hook 'org-mode-hook 'atchka-org/pretty-symbols-org-mode-appearance-hook)

;; Makes source blocks in org look prettier, and generally, org documents should
;; never exceed 80 columns or so. I use M-q (fill-column) constantly to enforce
;; this, which I think looks prettier and neater.
(add-hook 'window-configuration-change-hook (lambda ()
                                              (when (eq major-mode 'org-mode)
                                                (set-window-fringes
                                                 (selected-window) 30 34))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; ORG SOURCE BLOCKS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst atchka--org-block-header-height 0.1 "Height for org block lines.")

((when (require 'yasnippet nil t)
   (defun yas--show-org-block-lines ()
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
           (org-show-block-lines))))
     )

   (add-hook 'yas-before-expand-snippet-hook 'yas-show-org-block-lines)
   (add-hook 'yas-after-exit-snippet-hook 'org-hide-block-lines)
   ))

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
    (forward-line -1))
  )
(advice-add 'previous-line :before 'org-skip-source-previous-advice)

;; Abbreviations. The ~car~ of the list will be substitited for the ~cdr~.  This is
;; useful because Org is now set up to prettify =\word=, with the corresponding latex
;; symbol, "\alpha" becomes α. Don't want to do this directly because it will
;; interferes with inline latex
(define-abbrev-table 'org-abbrev-table    '(("alphaa" "\\alpha")
                                            ("inf" "∞")
                                            ("ar" "\\rightarrow")
                                            ("lambdaa" "\\lambda")
                                            ("sigmaa" "\\sigma")
                                            ("rhoo" "\\rho")
                                            ("betaa" "\\beta")
                                            ("etaa" "\\eta")
                                            ("deltaa" "\\delta")
                                            ("epsilonn" "\\epsilon")
                                            ("zetaa" "\\zeta")
                                            ("thetaa" "\\theta")
                                            ("iotaa" "\\iota")
                                            ("kappaa" "\\kappa")
                                            ("muu" "\\mu")
                                            ("nuu" "\\nu")
                                            ("xii" "\\xi")
                                            ("omicronn" "\\omicron")
                                            ("pii" "\\pi")
                                            ("tauu" "\\tau")
                                            ("upsilonn" "\\upsilon")
                                            ("phii" "\\phi")
                                            ("chii" "\\chi")
                                            ("psii" "\\psi")
                                            ("omegaa" "\\omega")
                                            ;; Capitals
                                            ("Alphaa" "\\Alpha")
                                            ("Inf" "∞")
                                            ("Ar" "\\Rightarrow")
                                            ("Lambdaa" "\\Lambda")
                                            ("Sigmaa" "\\Sigma")
                                            ("Rhoo" "\\Rho")
                                            ("Betaa" "\\Beta")
                                            ("Etaa" "\\Etaa")
                                            ("Deltaa" "\\Delta")
                                            ("Epsilonn" "\\Epsilon")
                                            ("Zetaa" "\\Zeta")
                                            ("Thetaa" "\\Theta")
                                            ("Iotaa" "\\Iota")
                                            ("Kappaa" "\\Kappa")
                                            ("Muu" "\\Mu")
                                            ("Nuu" "\\Nu")
                                            ("Xii" "\\Xi")
                                            ("Omicronn" "\\Omicron")
                                            ("Pii" "\\Pi")
                                            ("Tauu" "\\Tau")
                                            ("Upsilonn" "\\Upsilon")
                                            ("Phii" "\\Phi")
                                            ("Chii" "\\Chi")
                                            ("Psii" "\\Psi")
                                            ("Omegaa" "\\Omega")))

(provide 'atchka-org)

;;; atchka-org ends here
