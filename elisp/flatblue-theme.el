;; flatblue.el --- A flat colorscheme for emacs, with a dark blue background and vivid colours

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme flatblue "A flat colorscheme for emacs, with a dark blue background and vivid colours")
(let* ((colour-background_hard      "#0d1011")
       (colour-background           "#18191c") ;; 26262a or 1d1d22
       (colour-background_soft      "#212126")
       (colour-background_soft1     "#323037")
       (colour-background_soft2     "#3c383c")

       (colour-foreground_hard      "#ffffdf")
       (colour-foreground           "#fdf4d1")
       (colour-foreground_soft      "#f4e8ca")
       (colour-foreground_soft1     "#ebdbc2")
       (colour-foreground_soft2     "#a89994")

       (colour-comments             "#766f6f") ;; Comment gray

       (colour-red                  "#f24130")
       (colour-red_dark             "#d92817")
       (colour-green                "#71ba51")
       (colour-green_dark           "#3e871e")
       (colour-yellow_light         "#ffe000")
       (colour-yellow               "#f2bb13")
       (colour-yellow_dark          "#f39c12")
       (colour-blue_light           "#07b0ff")
       (colour-blue                 "#178ca6")
       (colour-blue_dark            "#005973")
       (colour-purple_light         "#ff465a")
       (colour-purple               "#dd465a")
       (colour-purple_dark          "#8f3f71")
       (colour-aqua                 "#67c5b4")
       (colour-aqua_dark            "#249991")
       (colour-orange               "#fe8019")
       (colour-orange_dark          "#fe5019")

       ;; Application specific
       (colour-delimiter-one        colour-blue_dark)
       (colour-delimiter-two        colour-purple_dark)
       (colour-delimiter-three      colour-aqua_dark)
       (colour-delimiter-four       colour-blue)

       ;; For coloured backgrounds
       (colour-light-foreground     colour-foreground)
       (colour-dark-foreground      colour-background)

       (colour-cursor               colour-blue)
       (colour-seperator            colour-background_hard)
       )

  (custom-theme-set-faces
   'flatblue

   ;; UI
   `(default                           ((t (:background ,colour-background :foreground ,colour-foreground))))
   `(cursor                            ((t (:background ,colour-cursor))))
   `(mode-line                         ((t (:box nil :background ,colour-background_soft1 :foreground ,colour-foreground_soft))))
   `(mode-line-inactive                ((t (:box nil :background ,colour-background_soft :foreground ,colour-foreground_soft2))))
   `(fringe                            ((t (:background ,colour-background))))
   `(linum                             ((t (:background ,colour-background :foreground ,colour-background_soft2))))
   `(hl-line                           ((t (:background ,colour-background_soft1))))
   `(region                            ((t (:background ,colour-background_soft2)))) ;;selection
   `(secondary-selection               ((t (:background ,colour-background_soft1))))
   `(minibuffer-prompt                 ((t (:background ,colour-background :foreground ,colour-green :bold t))))
   `(vertical-border                   ((t (:foreground ,colour-seperator))))

   ;; Built-in syntax
   `(font-lock-builtin-face            ((t (:foreground ,colour-orange))))
   `(font-lock-constant-face           ((t (:foreground ,colour-orange))))
   `(font-lock-comment-face            ((t (:foreground ,colour-comments :slant italic))))
   `(font-lock-doc-face                ((t (:foreground ,colour-green :slant italic))))
   `(font-lock-function-name-face      ((t (:foreground ,colour-yellow_light))))
   `(font-lock-keyword-face            ((t (:foreground ,colour-red))))
   `(font-lock-string-face             ((t (:foreground ,colour-green))))
   `(font-lock-variable-name-face      ((t (:foreground ,colour-blue))))
   `(font-lock-type-face               ((t (:foreground ,colour-blue_light))))
   `(font-lock-warning-face            ((t (:foreground ,colour-red :bold t))))

   ;; cquery
   `(cquery-sem-type-face              ((t (:inherit font-lock-type-face))))

   ;; lsp-ui
   `(lsp-xref-filename                 ((t (:foreground ,colour-foreground_soft))))
   `(lsp-xref-header                   ((t (:background ,colour-blue_dark :foreground ,colour-foreground))))
   `(lsp-xref-highlight                ((t (:background ,colour-background_soft2))))
   `(lsp-xref-line-number              ((t (:background ,colour-background_hard :inherit linum))))
   `(lsp-xref-list                     ((t (:background ,colour-background_hard :foreground ,colour-foreground_soft2))))
   `(lsp-xref-peek                     ((t (:background ,colour-background_hard))))

   ;; whitespace-mode
   `(whitespace-space                  ((t (:background ,colour-background :foreground ,colour-comments))))
   `(whitespace-hspace                 ((t (:background ,colour-background :foreground ,colour-comments))))
   `(whitespace-tab                    ((t (:background ,colour-background :foreground ,colour-comments))))
   `(whitespace-newline                ((t (:background ,colour-background :foreground ,colour-comments))))
   `(whitespace-trailing               ((t (:background ,colour-background_soft1 :foreground ,colour-red))))
   `(whitespace-line                   ((t (:background ,colour-background_soft1 :foreground ,colour-red))))
   `(whitespace-space-before-tab       ((t (:background ,colour-background :foreground ,colour-comments))))
   `(whitespace-indentation            ((t (:background ,colour-background :foreground ,colour-comments))))
   `(whitespace-empty                  ((t (:background nil :foreground nil))))
   `(whitespace-space-after-tab        ((t (:background ,colour-background :foreground ,colour-comments))))

   ;; RainbowDelimiters
   `(rainbow-delimiters-depth-1-face   ((t (:foreground ,colour-delimiter-one))))
   `(rainbow-delimiters-depth-2-face   ((t (:foreground ,colour-delimiter-two))))
   `(rainbow-delimiters-depth-3-face   ((t (:foreground ,colour-delimiter-three))))
   `(rainbow-delimiters-depth-4-face   ((t (:foreground ,colour-delimiter-four))))
   `(rainbow-delimiters-depth-5-face   ((t (:foreground ,colour-delimiter-one))))
   `(rainbow-delimiters-depth-6-face   ((t (:foreground ,colour-delimiter-two))))
   `(rainbow-delimiters-depth-7-face   ((t (:foreground ,colour-delimiter-three))))
   `(rainbow-delimiters-depth-8-face   ((t (:foreground ,colour-delimiter-four))))
   `(rainbow-delimiters-depth-9-face   ((t (:foreground ,colour-delimiter-one))))
   `(rainbow-delimiters-depth-10-face  ((t (:foreground ,colour-delimiter-two))))
   `(rainbow-delimiters-depth-11-face  ((t (:foreground ,colour-delimiter-three))))
   `(rainbow-delimiters-depth-12-face  ((t (:foreground ,colour-delimiter-four))))
   `(rainbow-delimiters-unmatched-face ((t (:background nil :foreground ,colour-foreground))))

   ;; linum-relative
   `(linum-relative-current-face       ((t (:background ,colour-background_soft1 :foreground ,colour-foreground_soft2))))

   ;; nlinum
   `(nlinum-current-line               ((t (:background ,colour-background_soft1 :foreground ,colour-foreground_soft2))))

   ;; Git gutter+
   `(git-gutter-fr+-added              ((t (:inherit fringe :foreground ,colour-green  ))))
   `(git-gutter-fr+-modified           ((t (:inherit fringe :foreground ,colour-orange ))))
   `(git-gutter-fr+-deleted              ((t (:inherit fringe :foreground ,colour-red    ))))

   ;; Highlight indentation mode
   `(highlight-indentation-current-column-face ((t (:background ,colour-background_soft2 ))))
   `(highlight-indentation-face                ((t (:background ,colour-background_soft1 ))))

   ;; Smartparens
   `(sp-pair-overlay-face              ((t (:background ,colour-background_soft2))))
                                        ;`(sp-wrap-overlay-face             ((t (:inherit sp-wrap-overlay-face))))
                                        ;`(sp-wrap-tag-overlay-face         ((t (:inherit sp-wrap-overlay-face))))
   `(sp-show-pair-match-face           ((t (:background ,colour-background_soft2)))) ;; Pair tags highlight
   `(sp-show-pair-mismatch-face        ((t (:background ,colour-red)))) ;; Highlight for bracket without pair

   ;; elscreen
   `(elscreen-tab-background-face      ((t (:box nil :background ,colour-background)))) ;; Tab bar, not the tabs
   `(elscreen-tab-control-face         ((t (:box nil :background ,colour-background_soft2 :foreground ,colour-red :underline nil)))) ;; The controls
   `(elscreen-tab-current-screen-face  ((t (:box nil :background ,colour-comments :foreground ,colour-background)))) ;; Current tab
   `(elscreen-tab-other-screen-face    ((t (:box nil :background ,colour-background_soft2 :foreground ,colour-foreground_soft2 :underline nil)))) ;; Inactive tab
   ;; ag (The Silver Searcher)
   `(ag-hit-face                       ((t (:foreground ,colour-blue))))
   `(ag-match-face                     ((t (:foreground ,colour-red))))

   ;; Diffs
   `(diff-changed                      ((t (:background nil :foreground ,colour-foreground_soft))))
   `(diff-added                        ((t (:background nil :foreground ,colour-green))))
   `(diff-removed                      ((t (:background nil :foreground ,colour-red))))
   `(diff-indicator-changed            ((t (:inherit diff-changed))))
   `(diff-indicator-added              ((t (:inherit diff-added))))
   `(diff-indicator-removed            ((t (:inherit diff-removed))))

   `(js2-warning                       ((t (:underline (:color ,colour-yellow :style wave)))))
   `(js2-error                         ((t (:underline (:color ,colour-red :style wave)))))
   `(js2-external-variable             ((t (:underline (:color ,colour-aqua :style wave)))))
   `(js2-jsdoc-tag                     ((t (:background nil :foreground ,colour-background_soft ))))
   `(js2-jsdoc-type                    ((t (:background nil :foreground ,colour-foreground_soft2 ))))
   `(js2-jsdoc-value                   ((t (:background nil :foreground ,colour-foreground_soft1 ))))
   `(js2-function-param                ((t (:background nil :foreground ,colour-aqua ))))
   `(js2-function-call                 ((t (:background nil :foreground ,colour-blue ))))
   `(js2-instance-member               ((t (:background nil :foreground ,colour-orange ))))
   `(js2-private-member                ((t (:background nil :foreground ,colour-yellow_dark ))))
   `(js2-private-function-call         ((t (:background nil :foreground ,colour-aqua_dark ))))
   `(js2-jsdoc-html-tag-name           ((t (:background nil :foreground ,colour-foreground_soft2 ))))
   `(js2-jsdoc-html-tag-delimiter      ((t (:background nil :foreground ,colour-foreground_soft1 ))))


   ;; popup
   `(popup-face                                ((t (:foreground ,colour-cursor :background ,colour-background_soft1))))
   `(popup-menu-mouse-face                     ((t (:foreground ,colour-foreground :background ,colour-cursor))))
   `(popup-menu-selection-face                 ((t (:foreground ,colour-foreground :background ,colour-background_soft))))
   `(popup-tip-face                            ((t (:foreground ,colour-foreground :background ,colour-background_soft2))))


   ;; helm
   `(helm-M-x-key                              ((t ( :foreground ,colour-orange  ))))
   `(helm-action                               ((t ( :foreground ,colour-light-foreground :underline t ))))
   `(helm-bookmark-addressbook                 ((t ( :foreground ,colour-red ))))
   `(helm-bookmark-directory                   ((t ( :foreground ,colour-purple ))))
   `(helm-bookmark-file                        ((t ( :foreground ,colour-blue_dark ))))
   `(helm-bookmark-gnus                        ((t ( :foreground ,colour-purple_dark ))))
   `(helm-bookmark-info                        ((t ( :foreground ,colour-aqua_dark ))))
   `(helm-bookmark-man                         ((t ( :foreground ,colour-orange_dark ))))
   `(helm-bookmark-w3m                         ((t ( :foreground ,colour-yellow ))))
   `(helm-buffer-directory                     ((t ( :foreground ,colour-light-foreground         :background ,colour-blue  ))))
   `(helm-buffer-not-saved                     ((t ( :foreground ,colour-red_dark ))))
   `(helm-buffer-process                       ((t ( :foreground ,colour-foreground_soft ))))
   `(helm-buffer-saved-out                     ((t ( :foreground ,colour-red ))))
   `(helm-buffer-size                          ((t ( :foreground ,colour-purple ))))
   `(helm-candidate-number                     ((t ( :foreground ,colour-green ))))
   `(helm-ff-directory                         ((t ( :foreground ,colour-purple ))))
   `(helm-ff-executable                        ((t ( :foreground ,colour-aqua_dark  ))))
   `(helm-ff-file                              ((t ( :foreground ,colour-orange_dark ))))
   `(helm-ff-invalid-symlink                   ((t ( :foreground ,colour-light-foreground         :background ,colour-red   ))))
   `(helm-ff-prefix                            ((t ( :foreground ,colour-dark-foreground         :background ,colour-yellow))))
   `(helm-ff-symlink                           ((t ( :foreground ,colour-orange ))))
   `(helm-grep-cmd-line                        ((t ( :foreground ,colour-green ))))
   `(helm-grep-file                            ((t ( :foreground ,colour-purple_dark ))))
   `(helm-grep-finish                          ((t ( :foreground ,colour-aqua_dark ))))
   `(helm-grep-lineno                          ((t ( :foreground ,colour-orange ))))
   `(helm-grep-match                           ((t ( :foreground ,colour-yellow ))))
   `(helm-grep-running                         ((t ( :foreground ,colour-red ))))
   `(helm-header                               ((t ( :foreground ,colour-aqua ))))
   `(helm-helper                               ((t ( :foreground ,colour-aqua ))))
   `(helm-history-deleted                      ((t ( :foreground ,colour-dark-foreground         :background ,colour-red   ))))
   `(helm-history-remote                       ((t ( :foreground ,colour-red_dark ))))
   `(helm-lisp-completion-info                 ((t ( :foreground ,colour-orange_dark ))))
   `(helm-lisp-show-completion                 ((t ( :foreground ,colour-red ))))
   `(helm-locate-finish                        ((t ( :foreground ,colour-light-foreground         :background ,colour-aqua  ))))
   `(helm-match                                ((t ( :foreground ,colour-orange ))))
   `(helm-moccur-buffer                        ((t ( :foreground ,colour-aqua :underline t                          ))))
   `(helm-prefarg                              ((t ( :foreground ,colour-aqua_dark ))))
   `(helm-selection                            ((t ( :foreground ,colour-light-foreground         :background ,colour-background_soft2        ))))
   `(helm-selection-line                       ((t ( :foreground ,colour-light-foreground         :background ,colour-background_soft2        ))))
   `(helm-separator                            ((t ( :foreground ,colour-red_dark ))))
   `(helm-source-header                        ((t ( :foreground ,colour-foreground_soft ))))
   `(helm-visible-mark                         ((t ( :foreground ,colour-dark-foreground         :background ,colour-foreground_soft1       ))))

   ;; company-mode
   `(company-scrollbar-bg              ((t (:background ,colour-background_soft1))))
   `(company-scrollbar-fg              ((t (:background ,colour-background_soft))))
   `(company-tooltip                   ((t (:background ,colour-background_soft))))
   `(company-tooltip-annotation        ((t (:foreground ,colour-green))))
   `(company-tooltip-selection         ((t (:foreground ,colour-purple))))
   `(company-tooltip-common            ((t (:foreground ,colour-blue :underline t))))
   `(company-tooltip-common-selection  ((t (:foreground ,colour-blue :underline t))))
   `(company-preview-common            ((t (:foreground ,colour-purple))))

   `(spaceline-evil-normal    ((t (:background ,colour-blue ))))
   `(spaceline-evil-insert    ((t (:background ,colour-green :foreground ,colour-background_soft2))))
   `(spaceline-evil-visual    ((t (:background ,colour-orange :foreground ,colour-background_soft1))))
   `(spaceline-evil-replace   ((t (:background ,colour-red ))))
   `(spaceline-evil-evilified ((t (:background ,colour-aqua ))))
   `(spaceline-evil-emacs     ((t (:background ,colour-purple ))))
   `(spaceline-evil-motion    ((t (:background ,colour-blue ))))
   `(spaceline-evil-operator  ((t (:background ,colour-blue ))))

   ;; Term
   `(term-color-black                  ((t (:foreground ,colour-dark-foreground))))
   `(term-color-blue                   ((t (:foreground ,colour-blue))))
   `(term-color-cyan                   ((t (:foreground ,colour-aqua))))
   `(term-color-green                  ((t (:foreground ,colour-green))))
   `(term-color-magenta                ((t (:foreground ,colour-purple))))
   `(term-color-red                    ((t (:foreground ,colour-red))))
   `(term-color-white                  ((t (:foreground ,colour-light-foreground))))
   `(term-color-yellow                 ((t (:foreground ,colour-yellow))))
   `(term-default-fg-color             ((t (:foreground ,colour-foreground))))
   `(term-default-bg-color             ((t (:background ,colour-background))))

   `(anzu-mode-line ((t (:inherit spaceline-default-face))))
   `(flycheck-error ((t (:underline ,colour-red))))
   `(flycheck-info ((t (:underline ,colour-green))))
   `(flycheck-warning ((t (:underline ,colour-orange))))
   `(git-gutter+-modified ((t (:foreground ,colour-orange :weight bold))))
   `(git-gutter+-separator ((t (:foreground ,colour-aqua :weight bold))))
   `(mode-line ((t (:background ,colour-background_soft1 :foreground ,colour-foreground_soft1 :box nil))))
   `(mode-line-highlight ((t (:box nil))))
   `(mode-line-inactive ((t (:box nil))))
   `(neo-vc-edited-face ((t (:foreground ,colour-orange))))
   `(neo-vc-up-to-date-face ((t (:inherit neo-file-link-face))))
   `(trailing-whitespace ((t (:background ,colour-background_soft))))

   ;; Smart-mode-line
   `(sml/global            ((t (:foreground ,colour-foreground_soft1 :inverse-video nil))))
   `(sml/modes             ((t (:foreground ,colour-green))))
   `(sml/filename          ((t (:foreground ,colour-red :weight bold))))
   `(sml/prefix            ((t (:foreground ,colour-foreground_soft))))
   `(sml/read-only         ((t (:foreground ,colour-blue))))
   `(persp-selected-face   ((t (:foreground ,colour-orange)))))

  (defun flatblue//linum-format (line)
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format "%%%dd " w) line) 'face 'linum)))

  (setq-default
   evil-insert-state-cursor    `(,colour-green bar)
   evil-normal-state-cursor    `(,colour-blue box)
   evil-visual-state-cursor    `(,colour-orange box)
   evil-replace-state-cursor   `(,colour-red box)
   evil-evilified-state-cursor `(,colour-aqua box)
   evil-emacs-state-cursor     `(,colour-purple bar)
   evil-operator-state-cursor  `(,colour-blue box)
   evil-motion-state-cursor    `(,colour-blue box))

  (custom-theme-set-variables
   'flatblue
   ;; export these colours
   `(fl-colour-green ,colour-green)
   `(fl-colour-blue ,colour-blue)
   `(fl-colour-orange ,colour-orange)
   `(fl-colour-red ,colour-red)
   `(fl-colour-aqua ,colour-aqua)
   `(fl-colour-purple ,colour-purple)

   `(left-fringe-width 10)
   `(right-fringe-width 10)
   `(git-gutter+-added-sign "▍")
   `(git-gutter+-modified-sign "▍")
   `(git-gutter+-deleted-sign "▍")
   `(fringes-outside-margins t)
   `(nlinum-highlight-current-line t)
   `(nlinum-format "%4d ")
   `(linum-format 'flatblue//linum-format)
   `(linum-relative-format "%4s ")
   ;; Evil

   `(ansi-color-names-vector
     [,colour-dark-foreground ,colour-red ,colour-green ,colour-yellow
                              ,colour-blue ,colour-purple ,colour-aqua
                              ,colour-light-foreground])))

(defun flatblue//set-git-fringe (fringe &optional corner)
  (if corner
      (fringe-helper-define fringe nil
        "XXXXXX..."
        "XXXXx...."
        "XXXx....."
        "XXx......"
        "Xx......."
        "x........"
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        "........."
        ".........")
    (fringe-helper-define fringe nil
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX...."
      "XXXXX....")
    ))

(defun flatblue-set-ansi-color-names-vector ()
  "Give comint and the like the same colours as the term colours we set."
  (setq ansi-color-names-vector
        [term-color-black term-color-red term-color-green term-color-yellow term-color-blue
                          term-color-purple term-color-aqua term-color-white]))

;;;###autoload
(and load-file-name
     (when (boundp 'custom-theme-load-path)
       (add-to-list 'custom-theme-load-path
                    (file-name-as-directory
                     (file-name-directory load-file-name)))))

;;;###autoload
(with-eval-after-load 'fringe-helper
  (with-eval-after-load 'git-gutter-fringe+
    (flatblue//set-git-fringe 'git-gutter-fr+-added)
    (flatblue//set-git-fringe 'git-gutter-fr+-modified)
    (flatblue//set-git-fringe 'git-gutter-fr+-deleted t)))

;;;###autoload
(with-eval-after-load 'linum-relative
  (add-hook 'linum-relative-mode-hook
            (lambda ()
              (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
                (make-variable-buffer-local 'linum-relative-format)
                (setq linum-relative-format (format "%%%ds " w))))))

(provide-theme 'flatblue)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; flatblue-theme.el ends here
