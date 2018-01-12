;; flatblue.el --- A flat colorscheme for emacs, with a dark blue background and vivid colours
(defgroup
  flatblue
  nil
  "Flatblue theme"
  :group nil)


(defcustom flatblue-font-family
  nil
  "The default face inherits from this - use it to set font"
  :type 'string
  :group 'flatblue)

(defcustom flatblue-font-size
  90
  "The default face inherits from this - use it to set font"
  :type 'number
  :group 'flatblue)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(defmacro flatblue-deftheme (name description palette &rest body)
  `(autothemer-deftheme
    ,name
    ,description
    ,palette

    ;; UI
    ((default                           (
                                         :background colour-background :foreground colour-foreground
                                         :family flatblue-font-family :height flatblue-font-size))
     (cursor                            (:background colour-cursor))
     (mode-line                         (:box nil :background colour-background_soft1 :foreground colour-foreground_soft))
     (mode-line-inactive                (:box nil :background colour-background_soft :foreground colour-foreground_soft2))
     (fringe                            (:background colour-background))
     (linum                             (:background colour-background :foreground colour-linum))
     (hl-line                           (:background colour-background_soft1))
     (region                            (:background colour-background_soft2)) ;;selection
     (secondary-selection               (:background colour-background_soft1))
     (minibuffer-prompt                 (:background colour-background :foreground colour-green :bold t))
     (vertical-border                   (:foreground colour-seperator))

     ;; Built-in syntax
     (font-lock-builtin-face            (:foreground colour-orange))
     (font-lock-constant-face           (:foreground colour-orange))
     (font-lock-comment-face            (:foreground colour-comments :slant 'italic))
     (font-lock-doc-face                (:foreground colour-green :slant 'italic))
     (font-lock-function-name-face      (:foreground colour-yellow_light))
     (font-lock-keyword-face            (:foreground colour-red))
     (font-lock-string-face             (:foreground colour-green))
     (font-lock-variable-name-face      (:foreground colour-blue))
     (font-lock-type-face               (:foreground colour-blue_light))
     (font-lock-warning-face            (:foreground colour-red :bold t))

     ;; cquery
     (cquery-sem-type-face              (:inherit 'font-lock-type-face))

     ;; lsp-ui
     (lsp-ui-peek-filename                 (:foreground colour-foreground_soft))
     (lsp-ui-peek-header                   (:background colour-blue_dark :foreground colour-foreground))
     (lsp-ui-peek-highlight                (:background colour-background_soft2))
     (lsp-ui-peek-line-number              (:background colour-background_hard :inherit 'linum))
     (lsp-ui-peek-list                     (:background colour-background_hard :foreground colour-foreground_soft2))
     (lsp-ui-peek-peek                     (:background colour-background_hard))

     ;; whitespace-mode
     (whitespace-space                  (:background colour-background :foreground colour-comments))
     (whitespace-hspace                 (:background colour-background :foreground colour-comments))
     (whitespace-tab                    (:background colour-background :foreground colour-comments))
     (whitespace-newline                (:background colour-background :foreground colour-comments))
     (whitespace-trailing               (:background colour-background_soft1 :foreground colour-red))
     (whitespace-line                   (:background colour-background_soft1 :foreground colour-red))
     (whitespace-space-before-tab       (:background colour-background :foreground colour-comments))
     (whitespace-indentation            (:background colour-background :foreground colour-comments))
     (whitespace-empty                  (:background nil :foreground nil))
     (whitespace-space-after-tab        (:background colour-background :foreground colour-comments))

     ;; RainbowDelimiters
     (rainbow-delimiters-depth-1-face   (:foreground colour-delimiter-one))
     (rainbow-delimiters-depth-2-face   (:foreground colour-delimiter-two))
     (rainbow-delimiters-depth-3-face   (:foreground colour-delimiter-three))
     (rainbow-delimiters-depth-4-face   (:foreground colour-delimiter-four))
     (rainbow-delimiters-depth-5-face   (:foreground colour-delimiter-one))
     (rainbow-delimiters-depth-6-face   (:foreground colour-delimiter-two))
     (rainbow-delimiters-depth-7-face   (:foreground colour-delimiter-three))
     (rainbow-delimiters-depth-8-face   (:foreground colour-delimiter-four))
     (rainbow-delimiters-depth-9-face   (:foreground colour-delimiter-one))
     (rainbow-delimiters-depth-10-face  (:foreground colour-delimiter-two))
     (rainbow-delimiters-depth-11-face  (:foreground colour-delimiter-three))
     (rainbow-delimiters-depth-12-face  (:foreground colour-delimiter-four))
     (rainbow-delimiters-unmatched-face (:background nil :foreground colour-foreground))

     ;; linum-relative
     (linum-relative-current-face       (:background colour-background_soft1 :foreground colour-foreground_soft2))

     ;; nlinum
     (nlinum-current-line               (:background colour-background_soft1 :foreground colour-foreground_soft2))

     ;; Git gutter+
     (git-gutter-fr+-added              (:inherit 'fringe :foreground colour-green  ))
     (git-gutter-fr+-modified           (:inherit 'fringe :foreground colour-orange ))
     (git-gutter-fr+-deleted              (:inherit 'fringe :foreground colour-red    ))

     ;; Highlight indentation mode
     (highlight-indentation-current-column-face (:background colour-background_soft2 ))
     (highlight-indentation-face                (:background colour-background_soft1 ))

     ;; Smartparens
     (sp-pair-overlay-face              (:background colour-background_soft2))
                                        ;(sp-wrap-overlay-face             (:inherit sp-wrap-overlay-face))
                                        ;(sp-wrap-tag-overlay-face         (:inherit sp-wrap-overlay-face))
     (sp-show-pair-match-face           (:background colour-background_soft2)) ;; Pair tags highlight
     (sp-show-pair-mismatch-face        (:background colour-red)) ;; Highlight for bracket without pair

     ;; elscreen
     (elscreen-tab-background-face      (:box nil :background colour-background)) ;; Tab bar not the tabs
     (elscreen-tab-control-face         (:box nil :background colour-background_soft2 :foreground colour-red :underline nil)) ;; The controls
     (elscreen-tab-current-screen-face  (:box nil :background colour-comments :foreground colour-background)) ;; Current tab
     (elscreen-tab-other-screen-face    (:box nil :background colour-background_soft2 :foreground colour-foreground_soft2 :underline nil)) ;; Inactive tab
     ;; ag (The Silver Searcher)
     (ag-hit-face                       (:foreground colour-blue))
     (ag-match-face                     (:foreground colour-red))

     ;; Diffs
     (diff-changed                      (:background nil :foreground colour-foreground_soft))
     (diff-added                        (:background nil :foreground colour-green))
     (diff-removed                      (:background nil :foreground colour-red))
     (diff-indicator-changed            (:inherit 'diff-changed))
     (diff-indicator-added              (:inherit 'diff-added))
     (diff-indicator-removed            (:inherit 'diff-removed))

     (js2-warning                       (:underline (:color colour-yellow :style 'wave)))
     (js2-error                         (:underline (:color colour-red :style 'wave)))
     (js2-external-variable             (:underline (:color colour-aqua :style' wave)))
     (js2-jsdoc-tag                     (:background nil :foreground colour-background_soft ))
     (js2-jsdoc-type                    (:background nil :foreground colour-foreground_soft2 ))
     (js2-jsdoc-value                   (:background nil :foreground colour-foreground_soft1 ))
     (js2-function-param                (:background nil :foreground colour-aqua ))
     (js2-function-call                 (:background nil :foreground colour-blue ))
     (js2-instance-member               (:background nil :foreground colour-orange ))
     (js2-private-member                (:background nil :foreground colour-yellow_dark ))
     (js2-private-function-call         (:background nil :foreground colour-aqua_dark ))
     (js2-jsdoc-html-tag-name           (:background nil :foreground colour-foreground_soft2 ))
     (js2-jsdoc-html-tag-delimiter      (:background nil :foreground colour-foreground_soft1 ))


     ;; popup
     (popup-face                                (:foreground colour-cursor :background colour-background_soft1))
     (popup-menu-mouse-face                     (:foreground colour-foreground :background colour-cursor))
     (popup-menu-selection-face                 (:foreground colour-foreground :background colour-background_soft))
     (popup-tip-face                            (:foreground colour-foreground :background colour-background_soft2))


     ;; helm
     (helm-M-x-key                              ( :foreground colour-orange  ))
     (helm-action                               ( :foreground colour-light-foreground :underline t ))
     (helm-bookmark-addressbook                 ( :foreground colour-red ))
     (helm-bookmark-directory                   ( :foreground colour-purple ))
     (helm-bookmark-file                        ( :foreground colour-blue_dark ))
     (helm-bookmark-gnus                        ( :foreground colour-purple_dark ))
     (helm-bookmark-info                        ( :foreground colour-aqua_dark ))
     (helm-bookmark-man                         ( :foreground colour-orange_dark ))
     (helm-bookmark-w3m                         ( :foreground colour-yellow ))
     (helm-buffer-directory                     ( :foreground colour-light-foreground         :background colour-blue  ))
     (helm-buffer-not-saved                     ( :foreground colour-red_dark ))
     (helm-buffer-process                       ( :foreground colour-foreground_soft ))
     (helm-buffer-saved-out                     ( :foreground colour-red ))
     (helm-buffer-size                          ( :foreground colour-purple ))
     (helm-candidate-number                     ( :foreground colour-green ))
     (helm-ff-directory                         ( :foreground colour-purple ))
     (helm-ff-executable                        ( :foreground colour-aqua_dark  ))
     (helm-ff-file                              ( :foreground colour-orange_dark ))
     (helm-ff-invalid-symlink                   ( :foreground colour-light-foreground         :background colour-red   ))
     (helm-ff-prefix                            ( :foreground colour-dark-foreground         :background colour-yellow))
     (helm-ff-symlink                           ( :foreground colour-orange ))
     (helm-grep-cmd-line                        ( :foreground colour-green ))
     (helm-grep-file                            ( :foreground colour-purple_dark ))
     (helm-grep-finish                          ( :foreground colour-aqua_dark ))
     (helm-grep-lineno                          ( :foreground colour-orange ))
     (helm-grep-match                           ( :foreground colour-yellow ))
     (helm-grep-running                         ( :foreground colour-red ))
     (helm-header                               ( :foreground colour-aqua ))
     (helm-helper                               ( :foreground colour-aqua ))
     (helm-history-deleted                      ( :foreground colour-dark-foreground         :background colour-red   ))
     (helm-history-remote                       ( :foreground colour-red_dark ))
     (helm-lisp-completion-info                 ( :foreground colour-orange_dark ))
     (helm-lisp-show-completion                 ( :foreground colour-red ))
     (helm-locate-finish                        ( :foreground colour-light-foreground         :background colour-aqua  ))
     (helm-match                                ( :foreground colour-orange ))
     (helm-moccur-buffer                        ( :foreground colour-aqua :underline t                          ))
     (helm-prefarg                              ( :foreground colour-aqua_dark ))
     (helm-selection                            ( :foreground colour-light-foreground         :background colour-background_soft2        ))
     (helm-selection-line                       ( :foreground colour-light-foreground         :background colour-background_soft2        ))
     (helm-separator                            ( :foreground colour-red_dark ))
     (helm-source-header                        ( :foreground colour-foreground_soft ))
     (helm-visible-mark                         ( :foreground colour-dark-foreground         :background colour-foreground_soft1       ))

     ;; company-mode
     (company-scrollbar-bg              (:background colour-background_soft1))
     (company-scrollbar-fg              (:background colour-background_soft))
     (company-tooltip                   (:background colour-background_soft))
     (company-tooltip-annotation        (:foreground colour-green))
     (company-tooltip-selection         (:foreground colour-purple))
     (company-tooltip-common            (:foreground colour-blue :underline t))
     (company-tooltip-common-selection  (:foreground colour-blue :underline t))
     (company-preview-common            (:foreground colour-purple))

     (spaceline-evil-normal    (:background colour-blue ))
     (spaceline-evil-insert    (:background colour-green :foreground colour-background_soft2))
     (spaceline-evil-visual    (:background colour-orange :foreground colour-background_soft1))
     (spaceline-evil-replace   (:background colour-red ))
     (spaceline-evil-evilified (:background colour-aqua ))
     (spaceline-evil-emacs     (:background colour-purple ))
     (spaceline-evil-motion    (:background colour-blue ))
     (spaceline-evil-operator  (:background colour-blue ))

     ;; Term
     (term-color-black                  (:foreground colour-dark-foreground))
     (term-color-blue                   (:foreground colour-blue))
     (term-color-cyan                   (:foreground colour-aqua))
     (term-color-green                  (:foreground colour-green))
     (term-color-magenta                (:foreground colour-purple))
     (term-color-red                    (:foreground colour-red))
     (term-color-white                  (:foreground colour-light-foreground))
     (term-color-yellow                 (:foreground colour-yellow))
     (term-default-fg-color             (:foreground colour-foreground))
     (term-default-bg-color             (:background colour-background))

     (anzu-mode-line         (:inherit 'spaceline-default-face))
     (flycheck-error         (:underline colour-red))
     (flycheck-info          (:underline colour-green))
     (flycheck-warning       (:underline colour-orange))
     (git-gutter+-modified   (:foreground colour-orange :weight 'bold))
     (git-gutter+-separator  (:foreground colour-aqua :weight 'bold))

     (mode-line              (:background colour-background_soft :foreground colour-foreground_soft :box nil))
     (powerline-active0      (:background colour-background_soft :foreground colour-foreground_soft :box nil))
     (powerline-active1      (:background colour-background_soft :foreground colour-foreground_soft :box nil))
     (powerline-active2      (:background colour-background_soft :foreground colour-foreground_soft :box nil))

     (mode-line-inactive     (:background colour-background_soft :foreground colour-foreground_soft2 :box nil))
     (powerline-inactive0    (:background colour-background_soft :foreground colour-foreground_soft2 :box nil))
     (powerline-inactive1    (:background colour-background_soft :foreground colour-foreground_soft2 :box nil))
     (powerline-inactive2    (:background colour-background_soft :foreground colour-foreground_soft2 :box nil))

     (mode-line-highlight    (:box nil))
     (neo-vc-edited-face     (:foreground colour-orange))
     (neo-vc-up-to-date-face (:inherit 'neo-file-link-face))
     (trailing-whitespace    (:background colour-background_soft))

     ;; Smart-mode-line
     (sml/global            (:foreground colour-foreground_soft1 :inverse-video nil))
     (sml/modes             (:foreground colour-green))
     (sml/filename          (:foreground colour-red :weight 'bold))
     (sml/prefix            (:foreground colour-foreground_soft))
     (sml/read-only         (:foreground colour-blue))
     (persp-selected-face   (:foreground colour-orange)))

    (defun flatblue//linum-format (line)
      (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
        (propertize (format (format "%%%dd " w) line) 'face 'linum)))

    (custom-theme-set-variables
     ',name
     `(evil-insert-state-cursor    '(,colour-green bar))
     `(evil-normal-state-cursor    '(,colour-blue box))
     `(evil-visual-state-cursor    '(,colour-orange box))
     `(evil-replace-state-cursor   '(,colour-red box))
     `(evil-evilified-state-cursor '(,colour-aqua box))
     `(evil-emacs-state-cursor     '(,colour-purple bar))
     `(evil-operator-state-cursor  '(,colour-blue box))
     `(evil-motion-state-cursor    '(,colour-blue box))
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
     `(lsp-ui-doc-background ,colour-background_soft)
     ;; Evil

     `(ansi-color-names-vector
       [,colour-dark-foreground
        ,colour-red
        ,colour-green
        ,colour-yellow
        ,colour-blue
        ,colour-purple
        ,colour-aqua
        ,colour-light-foreground]))

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
          "XXXXX....")))

    (with-eval-after-load 'fringe-helper
      (with-eval-after-load 'git-gutter-fringe+
        (flatblue//set-git-fringe 'git-gutter-fr+-added)
        (flatblue//set-git-fringe 'git-gutter-fr+-modified)
        (flatblue//set-git-fringe 'git-gutter-fr+-deleted t)))

    (with-eval-after-load 'linum-relative
      (add-hook 'linum-relative-mode-hook
                (lambda ()
                  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
                    (make-variable-buffer-local 'linum-relative-format)
                    (setq linum-relative-format (format "%%%ds " w))))))

    ,@body))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'flatblue)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; flatblue.el ends here
