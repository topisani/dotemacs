(setq lv-use-separator nil)

(defun my-hydra-key-doc-function (key key-width doc doc-width)
  (cond
   ((equal key " ") (format (format "%%-%ds" (+ 3 key-width doc-width)) doc))
   (t (format (format "%%%ds → %%%ds" key-width (- 0 doc-width)) key doc))))

(setq hydra-key-doc-function 'my-hydra-key-doc-function)

(require-package 'hydra)

(defun my-switch-action (fallback &rest props)
  "Performs an action based on the value of `dotemacs-switch-engine'."
  (cond
   ((and (eq dotemacs-switch-engine 'ivy) (plist-get props :ivy))
    (call-interactively (plist-get props :ivy)))
   ((and (eq dotemacs-switch-engine 'helm) (plist-get props :helm))
    (call-interactively (plist-get props :helm)))
   (t
    (if fallback
        (call-interactively fallback)
      (message "unsupported action")))))



(defhydra my-quit-hydra (:hint nil :exit t :idle 0.2)
  "Quit"
  ("q" save-buffers-kill-emacs "quit")
  ("r" (restart-emacs '("--debug-init")) "restart"))



(defhydra my-jump-hydra (:hint nil :exit t :idle 0.2)
  "
   jump   _i_ → outline in current buffer   _l_ → lines in current buffer
          _b_ → bookmarks                   _L_ → lines in all buffers
"
  ("i" (my-switch-action #'imenu :ivy #'counsel-imenu :helm #'helm-semantic-or-imenu))
  ("l" (my-switch-action nil     :ivy #'swiper        :helm #'helm-swoop))
  ("L" (my-switch-action nil     :ivy #'swiper-all    :helm #'helm-multi-swoop-all))
  ("b" bookmark-jump))



(defhydra my-search-hydra (:hint nil :exit t :idle 0.2)
  "Search "
  ("a" projectile-ag "ag" :column "Project")
  ("p" projectile-pt "pt" :column "Project")
  ("A" ag "ag" :column "Directory")
  ("P" pt-regexp "pt" :column "Directory")
  ("l" my-jump-hydra/lambda-l-and-exit "lines" :column "Buffer")
  ("L" my-jump-hydra/lambda-L-and-exit "lines" :column "buffers")
  ("g" my-google "google" :column "Web"))



(defhydra my-file-convert-hydra (:hint nil :exit t :idle 0.2)
  "
   convert to _d_ → dos
              _u_ → unix
"
  ("d" my-buffer-to-dos-format)
  ("u" my-buffer-to-unix-format))



(defhydra my-toggle-hydra (:hint nil :exit t :idle 0.2)
  "
      Toggles

      _a_ → aggressive indent   _s_ → flycheck       _r_ → read only      _t_ → truncate lines   _e_ → debug on error
      _f_ → auto-fill           _S_ → flyspell       _c_ → completion     _W_ → word wrap        _g_ → debug on quit
      _w_ → whitespace          _n_ → line numbers   _p_ → auto-pairing   _b_ → page break       _'_ → switch (%`dotemacs-switch-engine)
"
  ("a" aggressive-indent-mode)
  ("c" (if (eq dotemacs-completion-engine 'company)
           (call-interactively 'company-mode)
         (call-interactively 'auto-complete-mode)))
  ("t" toggle-truncate-lines)
  ("e" toggle-debug-on-error)
  ("g" toggle-debug-on-quit)
  ("b" page-break-lines-mode)
  ("s" flycheck-mode)
  ("S" flyspell-mode)
  ("w" whitespace-mode)
  ("W" toggle-word-wrap)
  ("r" read-only-mode)
  ("f" auto-fill-mode)
  ("n" linum-mode)
  ("p" (cond
        ((eq dotemacs-pair-engine 'emacs)
         (call-interactively #'electric-pair-mode))
        ((eq dotemacs-pair-engine 'smartparens)
         (call-interactively #'smartparens-global-mode))))
  ("'" (cond
        ((eq dotemacs-switch-engine 'ivy)  (my-activate-switch-engine 'helm))
        ((eq dotemacs-switch-engine 'helm) (my-activate-switch-engine 'ido))
        ((eq dotemacs-switch-engine 'ido)  (my-activate-switch-engine 'ivy)))
   :exit nil))

(provide 'init-hydras)
