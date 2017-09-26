;; flatblue.el --- A flat colorscheme for emacs, with a dark blue background and vivid colours

;; Copyright (c) 2013 Lee Machin
;; Copyright (c) 2013-2016 Greduan

;; Authors: Lee Machin <ljmachin@gmail.com>
;;          Greduan <me@greduan.com>
;; Maintainer: jasonm23 <jasonm23@gmail.com>
;; URL: http://github.com/Greduan/emacs-theme-flatblue
;; Package-Version: 20160514.658
;; Version: 0.16

;;; Commentary:

;; A port of the Flatblue colorscheme for Vim, built on top of the new built-in
;; theme support in Emacs 24.
;;
;; This theme contains my own modifications and it's a bit opinionated
;; sometimes, deviating from the original because of it. I try to stay true to
;; the original as much as possible, however. I only make changes where I would
;; have made the changes on the original.
;;
;; Since there is no direct equivalent in syntax highlighting from Vim to Emacs
;; some stuff may look different, especially in stuff like JS2-mode, where it
;; adds stuff that Vim doesn't have, in terms of syntax.

;;; Credits:

;; Pavel Pertsev created the original theme for Vim, on which this port
;; is based.

;; Lee Machin created the first port of the original theme, which I'm working on
;; to make better and more feature complete.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme flatblue "A flat colorscheme for emacs, with a dark blue background and vivid colours")
(let ((flatblue-darker      (if (display-graphic-p) "#0d1011" "color-234"))
      (flatblue-dark0_hard  (if (display-graphic-p) "#1d2021" "color-234"))
      (flatblue-dark0       (if (display-graphic-p) "#26262a" "color-235"))
      (flatblue-dark0_soft  (if (display-graphic-p) "#323037" "color-236"))
      (flatblue-dark1       (if (display-graphic-p) "#3c383c" "color-237"))
      (flatblue-dark2       (if (display-graphic-p) "#47494b" "color-239"))
      (flatblue-dark3       (if (display-graphic-p) "#645c5f" "color-241"))
      (flatblue-dark4       (if (display-graphic-p) "#766f6f" "color-243"))

      (flatblue-medium      (if (display-graphic-p) "#37383f" "color-245")) ;; or 244

      (flatblue-light0_hard (if (display-graphic-p) "#ffffd8" "color-230"))
      (flatblue-light0      (if (display-graphic-p) "#fdf4d1" "color-229"))
      (flatblue-light0_soft (if (display-graphic-p) "#f4e8ca" "color-228"))
      (flatblue-light1      (if (display-graphic-p) "#ebdbc2" "color-223"))
      (flatblue-light2      (if (display-graphic-p) "#d5c4b1" "color-250"))
      (flatblue-light3      (if (display-graphic-p) "#bdaea3" "color-248"))
      (flatblue-light4      (if (display-graphic-p) "#a89994" "color-246"))

      (flatblue-bright_red     (if (display-graphic-p) "#f24130" "color-167"))
      (flatblue-bright_green   (if (display-graphic-p) "#71ba51" "color-142"))
      (flatblue-bright_yellow  (if (display-graphic-p) "#f2bb13" "color-214"))
      (flatblue-bright_blue    (if (display-graphic-p) "#178ca6" "color-109"))
      (flatblue-bright_purple  (if (display-graphic-p) "#bb3658" "color-175"))
      (flatblue-bright_aqua    (if (display-graphic-p) "#67c5b4" "color-108"))
      (flatblue-bright_orange  (if (display-graphic-p) "#fe8019" "color-208"))

      ;; neutral, no 256-color code, requested, nice work-around meanwhile
      (flatblue-neutral_red    (if (display-graphic-p) "#f24130" "#d75f5f"))
      (flatblue-neutral_green  (if (display-graphic-p) "#71ba51" "#afaf00"))
      (flatblue-neutral_yellow (if (display-graphic-p) "#f2bb13" "#ffaf00"))
      (flatblue-neutral_blue   (if (display-graphic-p) "#178ca6" "#87afaf"))
      (flatblue-neutral_purple (if (display-graphic-p) "#bb3658" "#d787af"))
      (flatblue-neutral_aqua   (if (display-graphic-p) "#67c5b4" "#87af87"))
      (flatblue-neutral_orange (if (display-graphic-p) "#fe8019" "#ff8700"))

      (flatblue-faded_red      (if (display-graphic-p) "#9d0006" "color-88"))
      (flatblue-faded_green    (if (display-graphic-p) "#79740e" "color-100"))
      (flatblue-faded_yellow   (if (display-graphic-p) "#b57614" "color-136"))
      (flatblue-faded_blue     (if (display-graphic-p) "#076678" "color-24"))
      (flatblue-faded_purple   (if (display-graphic-p) "#8f3f71" "color-96"))
      (flatblue-faded_aqua     (if (display-graphic-p) "#427b58" "color-66"))
      (flatblue-faded_orange   (if (display-graphic-p) "#af3a03" "color-130"))

      (flatblue-delimiter-one    (if (display-graphic-p) "#005973" "color-30"))
      (flatblue-delimiter-two    (if (display-graphic-p) "#7e3661" "color-168"))
      (flatblue-delimiter-three  (if (display-graphic-p) "#67c5b4" "color-108"))
      (flatblue-delimiter-four   (if (display-graphic-p) "#d65d0e" "color-166"))
      (flatblue-white            (if (display-graphic-p) "#FFFFFF" "white"))
      (flatblue-black            (if (display-graphic-p) "#000000" "black"))
      (flatblue-sienna           (if (display-graphic-p) "#DD6F48" "sienna"))
      (flatblue-darkslategray4   (if (display-graphic-p) "#528B8B" "DarkSlateGray4"))
      (flatblue-lightblue4       (if (display-graphic-p) "#66999D" "LightBlue4"))
      (flatblue-burlywood4       (if (display-graphic-p) "#BBAA97" "burlywood4"))
      (flatblue-aquamarine4      (if (display-graphic-p) "#178CA6" "aquamarine4"))
      (flatblue-turquoise4       (if (display-graphic-p) "#61ACBB" "turquoise4")))

  (custom-theme-set-faces
   'flatblue

   ;; UI
   `(default                           ((t (:background ,flatblue-dark0 :foreground ,flatblue-light0))))
   `(cursor                            ((t (:background ,flatblue-light0))))
   `(mode-line                         ((t (:box nil :background ,flatblue-dark2 :foreground ,flatblue-light2))))
   `(mode-line-inactive                ((t (:box nil :background ,flatblue-dark1 :foreground ,flatblue-light4))))
   `(fringe                            ((t (:background ,flatblue-dark0))))
   `(linum                             ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark2))))
   `(hl-line                           ((t (:background ,flatblue-dark1))))
   `(region                            ((t (:background ,flatblue-dark2)))) ;;selection
   `(secondary-selection               ((t (:background ,flatblue-dark1))))
   `(minibuffer-prompt                 ((t (:background ,flatblue-dark0 :foreground ,flatblue-neutral_green :bold t))))
   `(vertical-border                   ((t (:foreground ,flatblue-darker))))

   ;; Built-in syntax
   `(font-lock-builtin-face            ((t (:foreground ,flatblue-neutral_orange))))
   `(font-lock-constant-face           ((t (:foreground "dark orange"))))
   `(font-lock-comment-face            ((t (:foreground ,flatblue-dark4))))
   `(font-lock-function-name-face      ((t (:foreground ,flatblue-neutral_yellow))))
   `(font-lock-keyword-face            ((t (:foreground ,flatblue-neutral_red))))
   `(font-lock-string-face             ((t (:foreground ,flatblue-neutral_green))))
   `(font-lock-variable-name-face      ((t (:foreground ,flatblue-neutral_blue))))
   `(font-lock-type-face               ((t (:foreground "DeepSkyBlue3"))))
   `(font-lock-warning-face            ((t (:foreground ,flatblue-neutral_red :bold t))))

   ;; whitespace-mode
   `(whitespace-space                  ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))
   `(whitespace-hspace                 ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))
   `(whitespace-tab                    ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))
   `(whitespace-newline                ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))
   `(whitespace-trailing               ((t (:background ,flatblue-dark1 :foreground ,flatblue-neutral_red))))
   `(whitespace-line                   ((t (:background ,flatblue-dark1 :foreground ,flatblue-neutral_red))))
   `(whitespace-space-before-tab       ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))
   `(whitespace-indentation            ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))
   `(whitespace-empty                  ((t (:background nil :foreground nil))))
   `(whitespace-space-after-tab        ((t (:background ,flatblue-dark0 :foreground ,flatblue-dark4))))

   ;; RainbowDelimiters
   `(rainbow-delimiters-depth-1-face   ((t (:foreground ,flatblue-delimiter-one))))
   `(rainbow-delimiters-depth-2-face   ((t (:foreground ,flatblue-delimiter-two))))
   `(rainbow-delimiters-depth-3-face   ((t (:foreground ,flatblue-delimiter-three))))
   `(rainbow-delimiters-depth-4-face   ((t (:foreground ,flatblue-delimiter-four))))
   `(rainbow-delimiters-depth-5-face   ((t (:foreground ,flatblue-delimiter-one))))
   `(rainbow-delimiters-depth-6-face   ((t (:foreground ,flatblue-delimiter-two))))
   `(rainbow-delimiters-depth-7-face   ((t (:foreground ,flatblue-delimiter-three))))
   `(rainbow-delimiters-depth-8-face   ((t (:foreground ,flatblue-delimiter-four))))
   `(rainbow-delimiters-depth-9-face   ((t (:foreground ,flatblue-delimiter-one))))
   `(rainbow-delimiters-depth-10-face  ((t (:foreground ,flatblue-delimiter-two))))
   `(rainbow-delimiters-depth-11-face  ((t (:foreground ,flatblue-delimiter-three))))
   `(rainbow-delimiters-depth-12-face  ((t (:foreground ,flatblue-delimiter-four))))
   `(rainbow-delimiters-unmatched-face ((t (:background nil :foreground ,flatblue-light0))))

   ;; linum-relative
   `(linum-relative-current-face       ((t (:background ,flatblue-dark1 :foreground ,flatblue-light4))))

   ;; nlinum
   `(nlinum-current-line               ((t (:background ,flatblue-dark1 :foreground ,flatblue-light4))))

   ;; Git gutter+
   `(git-gutter-fr+-added              ((t (:inherit fringe :foreground ,flatblue-neutral_green  ))))
   `(git-gutter-fr+-modified           ((t (:inherit fringe :foreground ,flatblue-neutral_orange ))))
   `(git-gutter-fr+-deleted              ((t (:inherit fringe :foreground ,flatblue-neutral_red    ))))

   ;; Highlight indentation mode
   `(highlight-indentation-current-column-face ((t (:background ,flatblue-dark2 ))))
   `(highlight-indentation-face                ((t (:background ,flatblue-dark1 ))))

   ;; Smartparens
   `(sp-pair-overlay-face              ((t (:background ,flatblue-dark2))))
                                        ;`(sp-wrap-overlay-face             ((t (:inherit sp-wrap-overlay-face))))
                                        ;`(sp-wrap-tag-overlay-face         ((t (:inherit sp-wrap-overlay-face))))
   `(sp-show-pair-match-face           ((t (:background ,flatblue-dark2)))) ;; Pair tags highlight
   `(sp-show-pair-mismatch-face        ((t (:background ,flatblue-neutral_red)))) ;; Highlight for bracket without pair

   ;; elscreen
   `(elscreen-tab-background-face      ((t (:box nil :background ,flatblue-dark0)))) ;; Tab bar, not the tabs
   `(elscreen-tab-control-face         ((t (:box nil :background ,flatblue-dark2 :foreground ,flatblue-neutral_red :underline nil)))) ;; The controls
   `(elscreen-tab-current-screen-face  ((t (:box nil :background ,flatblue-dark4 :foreground ,flatblue-dark0)))) ;; Current tab
   `(elscreen-tab-other-screen-face    ((t (:box nil :background ,flatblue-dark2 :foreground ,flatblue-light4 :underline nil)))) ;; Inactive tab
   ;; ag (The Silver Searcher)
   `(ag-hit-face                       ((t (:foreground ,flatblue-neutral_blue))))
   `(ag-match-face                     ((t (:foreground ,flatblue-neutral_red))))

   ;; Diffs
   `(diff-changed                      ((t (:background nil :foreground ,flatblue-light1))))
   `(diff-added                        ((t (:background nil :foreground ,flatblue-neutral_green))))
   `(diff-removed                      ((t (:background nil :foreground ,flatblue-neutral_red))))
   `(diff-indicator-changed            ((t (:inherit diff-changed))))
   `(diff-indicator-added              ((t (:inherit diff-added))))
   `(diff-indicator-removed            ((t (:inherit diff-removed))))

   `(js2-warning                       ((t (:underline (:color ,flatblue-bright_yellow :style wave)))))
   `(js2-error                         ((t (:underline (:color ,flatblue-bright_red :style wave)))))
   `(js2-external-variable             ((t (:underline (:color ,flatblue-bright_aqua :style wave)))))
   `(js2-jsdoc-tag                     ((t (:background nil :foreground ,flatblue-medium ))))
   `(js2-jsdoc-type                    ((t (:background nil :foreground ,flatblue-light4 ))))
   `(js2-jsdoc-value                   ((t (:background nil :foreground ,flatblue-light3 ))))
   `(js2-function-param                ((t (:background nil :foreground ,flatblue-bright_aqua ))))
   `(js2-function-call                 ((t (:background nil :foreground ,flatblue-bright_blue ))))
   `(js2-instance-member               ((t (:background nil :foreground ,flatblue-bright_orange ))))
   `(js2-private-member                ((t (:background nil :foreground ,flatblue-faded_yellow ))))
   `(js2-private-function-call         ((t (:background nil :foreground ,flatblue-faded_aqua ))))
   `(js2-jsdoc-html-tag-name           ((t (:background nil :foreground ,flatblue-light4 ))))
   `(js2-jsdoc-html-tag-delimiter      ((t (:background nil :foreground ,flatblue-light3 ))))


   ;; popup
   `(popup-face                                ((t (:foreground ,flatblue-light1 :background ,flatblue-dark1))))
   `(popup-menu-mouse-face                     ((t (:foreground ,flatblue-light0 :background ,flatblue-faded_green))))
   `(popup-menu-selection-face                 ((t (:foreground ,flatblue-light0 :background ,flatblue-faded_green))))
   `(popup-tip-face                            ((t (:foreground ,flatblue-light2 :background ,flatblue-dark2))))


   ;; helm
   `(helm-M-x-key                              ((t ( :foreground ,flatblue-neutral_orange  ))))
   `(helm-action                               ((t ( :foreground ,flatblue-white :underline t ))))
   `(helm-bookmark-addressbook                 ((t ( :foreground ,flatblue-neutral_red ))))
   `(helm-bookmark-directory                   ((t ( :foreground ,flatblue-bright_purple ))))
   `(helm-bookmark-file                        ((t ( :foreground ,flatblue-faded_blue ))))
   `(helm-bookmark-gnus                        ((t ( :foreground ,flatblue-faded_purple ))))
   `(helm-bookmark-info                        ((t ( :foreground ,flatblue-turquoise4 ))))
   `(helm-bookmark-man                         ((t ( :foreground ,flatblue-sienna ))))
   `(helm-bookmark-w3m                         ((t ( :foreground ,flatblue-neutral_yellow ))))
   `(helm-buffer-directory                     ((t ( :foreground ,flatblue-white         :background ,flatblue-bright_blue  ))))
   `(helm-buffer-not-saved                     ((t ( :foreground ,flatblue-faded_red ))))
   `(helm-buffer-process                       ((t ( :foreground ,flatblue-burlywood4 ))))
   `(helm-buffer-saved-out                     ((t ( :foreground ,flatblue-bright_red ))))
   `(helm-buffer-size                          ((t ( :foreground ,flatblue-bright_purple ))))
   `(helm-candidate-number                     ((t ( :foreground ,flatblue-neutral_green ))))
   `(helm-ff-directory                         ((t ( :foreground ,flatblue-neutral_purple ))))
   `(helm-ff-executable                        ((t ( :foreground ,flatblue-turquoise4  ))))
   `(helm-ff-file                              ((t ( :foreground ,flatblue-sienna ))))
   `(helm-ff-invalid-symlink                   ((t ( :foreground ,flatblue-white         :background ,flatblue-bright_red   ))))
   `(helm-ff-prefix                            ((t ( :foreground ,flatblue-black         :background ,flatblue-neutral_yellow))))
   `(helm-ff-symlink                           ((t ( :foreground ,flatblue-neutral_orange ))))
   `(helm-grep-cmd-line                        ((t ( :foreground ,flatblue-neutral_green ))))
   `(helm-grep-file                            ((t ( :foreground ,flatblue-faded_purple ))))
   `(helm-grep-finish                          ((t ( :foreground ,flatblue-turquoise4 ))))
   `(helm-grep-lineno                          ((t ( :foreground ,flatblue-neutral_orange ))))
   `(helm-grep-match                           ((t ( :foreground ,flatblue-neutral_yellow ))))
   `(helm-grep-running                         ((t ( :foreground ,flatblue-neutral_red ))))
   `(helm-header                               ((t ( :foreground ,flatblue-aquamarine4 ))))
   `(helm-helper                               ((t ( :foreground ,flatblue-aquamarine4 ))))
   `(helm-history-deleted                      ((t ( :foreground ,flatblue-black         :background ,flatblue-bright_red   ))))
   `(helm-history-remote                       ((t ( :foreground ,flatblue-faded_red ))))
   `(helm-lisp-completion-info                 ((t ( :foreground ,flatblue-faded_orange ))))
   `(helm-lisp-show-completion                 ((t ( :foreground ,flatblue-bright_red ))))
   `(helm-locate-finish                        ((t ( :foreground ,flatblue-white         :background ,flatblue-aquamarine4  ))))
   `(helm-match                                ((t ( :foreground ,flatblue-neutral_orange ))))
   `(helm-moccur-buffer                        ((t ( :foreground ,flatblue-bright_aqua :underline t                          ))))
   `(helm-prefarg                              ((t ( :foreground ,flatblue-turquoise4 ))))
   `(helm-selection                            ((t ( :foreground ,flatblue-white         :background ,flatblue-dark2        ))))
   `(helm-selection-line                       ((t ( :foreground ,flatblue-white         :background ,flatblue-dark2        ))))
   `(helm-separator                            ((t ( :foreground ,flatblue-faded_red ))))
   `(helm-source-header                        ((t ( :foreground ,flatblue-light2 ))))
   `(helm-visible-mark                         ((t ( :foreground ,flatblue-black         :background ,flatblue-light3       ))))

   ;; company-mode
   `(company-scrollbar-bg              ((t (:background ,flatblue-dark1))))
   `(company-scrollbar-fg              ((t (:background ,flatblue-dark0_soft))))
   `(company-tooltip                   ((t (:background ,flatblue-dark0_soft))))
   `(company-tooltip-annotation        ((t (:foreground ,flatblue-neutral_green))))
   `(company-tooltip-selection         ((t (:foreground ,flatblue-neutral_purple))))
   `(company-tooltip-common            ((t (:foreground ,flatblue-neutral_blue :underline t))))
   `(company-tooltip-common-selection  ((t (:foreground ,flatblue-neutral_blue :underline t))))
   `(company-preview-common            ((t (:foreground ,flatblue-neutral_purple))))

   `(spaceline-evil-normal    ((t (:background ,flatblue-neutral_blue ))))
   `(spaceline-evil-insert    ((t (:background ,flatblue-neutral_green :foreground ,flatblue-dark2))))
   `(spaceline-evil-visual    ((t (:background ,flatblue-neutral_orange :foreground ,flatblue-dark1))))
   `(spaceline-evil-replace   ((t (:background ,flatblue-neutral_red ))))
   `(spaceline-evil-evilified ((t (:background ,flatblue-neutral_aqua ))))
   `(spaceline-evil-emacs     ((t (:background ,flatblue-neutral_purple ))))
   `(spaceline-evil-motion    ((t (:background ,flatblue-neutral_blue ))))
   `(spaceline-evil-operator  ((t (:background ,flatblue-neutral_blue ))))

   ;; Term
   `(term-color-black                  ((t (:foreground ,flatblue-dark1))))
   `(term-color-blue                   ((t (:foreground ,flatblue-neutral_blue))))
   `(term-color-cyan                   ((t (:foreground ,flatblue-neutral_aqua))))
   `(term-color-green                  ((t (:foreground ,flatblue-neutral_green))))
   `(term-color-magenta                ((t (:foreground ,flatblue-neutral_purple))))
   `(term-color-red                    ((t (:foreground ,flatblue-neutral_red))))
   `(term-color-white                  ((t (:foreground ,flatblue-light1))))
   `(term-color-yellow                 ((t (:foreground ,flatblue-neutral_yellow))))
   `(term-default-fg-color             ((t (:foreground ,flatblue-light0))))
   `(term-default-bg-color             ((t (:background ,flatblue-dark0))))

   ;; Smart-mode-line
   `(sml/global            ((t (:foreground ,flatblue-burlywood4 :inverse-video nil))))
   `(sml/modes             ((t (:foreground ,flatblue-bright_green))))
   `(sml/filename          ((t (:foreground ,flatblue-bright_red :weight bold))))
   `(sml/prefix            ((t (:foreground ,flatblue-light1))))
   `(sml/read-only         ((t (:foreground ,flatblue-neutral_blue))))
   `(persp-selected-face   ((t (:foreground ,flatblue-neutral_orange)))))

  (defun flatblue//linum-format (line)
    (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
      (propertize (format (format "%%%dd " w) line) 'face 'linum)))

  (custom-theme-set-variables
   'flatblue
   `(left-fringe-width 10)
   `(right-fringe-width 10)
   `(git-gutter+-added-sign "▍")
   `(git-gutter+-modified-sign "▍")
   `(git-gutter+-deleted-sign "▍")
   `(fringes-outside-margins t)
   `(nlinum-highlight-current-line t)
   `(nlinum-format "%4d ")
   ;; Evil
   `(evil-normal-state-cursor '(,flatblue-neutral_blue box))
   `(evil-insert-state-cursor '(,flatblue-neutral_green bar))
   `(evil-visual-state-cursor '(,flatblue-neutral_orange box))
   `(evil-replace-state-cursor '(,flatblue-neutral_red box))
   `(evil-evilified-state-cursor '(,flatblue-neutral_aqua box))
   `(evil-emacs-state-cursor '(,flatblue-neutral_purple box))
   `(evil-motion-state-cursor '(,flatblue-neutral_blue box))
   `(evil-operator-state-cursor '(,flatblue-neutral_blue box))

   `(linum-format 'flatblue//linum-format)
   `(ansi-color-names-vector [,flatblue-dark1 ,flatblue-neutral_red
                                              ,flatblue-neutral_green ,flatblue-neutral_yellow ,flatblue-neutral_blue
                                              ,flatblue-neutral_purple ,flatblue-neutral_aqua ,flatblue-light1])))

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

(provide-theme 'flatblue)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; flatblue-theme.el ends here
