(require 'flatblue)

(flatblue-deftheme
 flatblue
 "A flat colorscheme for emacs, with a dark blue background and vivid colours"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256
  (colour-background_hard      "#0d1011")
  (colour-background           "#18191c")
  (colour-background_soft      "#1E1F21") ;; 26262a or 1d1d22
  (colour-background_soft1     "#212126")
  (colour-background_soft2     "#323037")
  (colour-background_soft3     "#3c383c")

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

  (colour-linum                colour-background_soft3)
  (colour-cursor               colour-blue)
  (colour-seperator            colour-background_hard)))

(provide-theme 'flatblue)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:
