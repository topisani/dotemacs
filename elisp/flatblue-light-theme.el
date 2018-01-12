(require 'flatblue)

(flatblue-deftheme
 flatblue-light
 "A flat colorscheme for emacs, with a white background and vivid colours"
 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  (colour-background_hard      "#fdf4df")
  (colour-background           "#ffffef")
  (colour-background_soft      "#fdf4df")
  (colour-background_soft1     "#f4e8da")
  (colour-background_soft2     "#ebdbd2")
  (colour-background_soft3     "#d8d9d4")

  (colour-foreground_hard      "#0d1011")
  (colour-foreground           "#18191c")
  (colour-foreground_soft      "#1E1F21") ;; 26262a or 1d1d22
  (colour-foreground_soft1     "#212126")
  (colour-foreground_soft2     "#323037")

  (colour-comments             "#766f6f") ;; Comment gray

  (colour-red_dark             "#f24130")
  (colour-red                  "#d92817")
  (colour-green_dark           "#71ba51")
  (colour-green                "#3e871e")
  (colour-yellow_light         "#e38b12")
  (colour-yellow               "#f2bb13")
  (colour-yellow_dark          "#ffe000")
  (colour-blue_light           "#005973")
  (colour-blue                 "#178ca6")
  (colour-blue_dark            "#07b0ff")
  (colour-purple_light         "#8f3f71")
  (colour-purple               "#dd465a")
  (colour-purple_dark          "#ff465a")
  (colour-aqua                 "#249991")
  (colour-aqua_dark            "#67c5b4")
  (colour-orange               "#fe5019")
  (colour-orange_dark          "#fe8019")

  ;; Application specific
  (colour-delimiter-one        colour-blue_dark)
  (colour-delimiter-two        colour-purple_dark)
  (colour-delimiter-three      colour-aqua_dark)
  (colour-delimiter-four       colour-blue)

  ;; For coloured backgrounds
  (colour-light-foreground     colour-foreground)
  (colour-dark-foreground      colour-background)

  (colour-linum                colour-comments)
  (colour-cursor               colour-blue)
  (colour-seperator            colour-background_hard)))

(provide-theme 'flatblue-light)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:
