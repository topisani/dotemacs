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



(setq my-errors-hydra/flycheck nil)
(defun my-errors-hydra/target-list ()
  (if my-errors-hydra/flycheck
      'flycheck
    'emacs))
(defhydra my-errors-hydra (:hint nil :idle 0.2)
  "
   errors:  navigation                 flycheck
            -----------------------    ---------------
            _j_ → next error             _l_ → list errors
            _k_ → previous error         _?_ → describe checker
            _t_ → toggle list (%(my-errors-hydra/target-list))
"
  ("j" (if my-errors-hydra/flycheck
           (call-interactively #'flycheck-next-error)
         (call-interactively #'next-error)))
  ("k" (if my-errors-hydra/flycheck
           (call-interactively #'flycheck-previous-error)
         (call-interactively #'previous-error)))
  ("t" (setq my-errors-hydra/flycheck (not my-errors-hydra/flycheck)))
  ("?" flycheck-describe-checker)
  ("l" flycheck-list-errors :exit t))



(defhydra my-quit-hydra (:hint nil :exit t :idle 0.2)
  "Quit"
  ("q" save-buffers-kill-emacs "quit")
  ("r" (restart-emacs '("--debug-init")) "restart"))



(defhydra my-buffer-hydra (:hint nil :exit t :columns 3 :idle 0.2)
  "Buffers"
  ("s" my-goto-scratch-buffer "goto scratch")
  ("d" kill-this-buffer "delete buffer")
  ("m" (switch-to-buffer "*Messages*") "goto messages")
  ("b" (my-switch-action #'switch-to-buffer :ivy #'my-ivy-mini :helm #'helm-mini) "buffers")
  ("e" erase-buffer "erase buffer")
  ("E" (let ((inhibit-read-only t)) (erase-buffer)) "erase buffer (force)"))



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


(defhydra my-file-hydra (:hint nil :exit t :columns 5 :idle 0.2)
  "Files"
  ("D" my-delete-current-buffer-file "delete")
  ("R" my-rename-current-buffer-file "rename")
  ("f" (my-switch-action #'find-file :ivy #'counsel-find-file :helm #'helm-find-files) "find files")
  ("r" (my-switch-action #'recentf   :ivy #'ivy-recentf       :helm #'helm-recentf) "recent files")
  ("y" my-copy-file-name-to-clipboard "copy filename")
  ("E" my-find-file-as-root "edit as root")
  ("c" copy-file "copy file")
  ("t" neotree-toggle "toggle neotree  ")
  ("C" my-file-convert-hydra/body "convert")
  ("s" save-buffer "save"))



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
  ("n" global-linum-mode)
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



(defhydra my-helm-hydra (:hint nil :exit t :idle 0.2)
  "
   helm:   _a_ → apropos   _m_ → bookmarks   _y_ → kill-ring  _l_ → swoop
           _b_ → mini      _p_ → projectile  _d_ → dash       _L_ → swoop (multi)
           _e_ → recentf   _r_ → register    _x_ → M-x
           _f_ → files     _t_ → tags
"
  ("a" helm-apropos)
  ("b" helm-mini)
  ("d" helm-dash)
  ("e" helm-recentf)
  ("f" helm-find-files)
  ("m" helm-bookmarks)
  ("p" helm-projectile)
  ("r" helm-register)
  ("t" helm-etags-select)
  ("x" helm-M-x)
  ("l" helm-swoop)
  ("L" helm-multi-swoop-all)
  ("y" helm-show-kill-ring))



(defhydra my-ivy-hydra (:hint nil :exit t :idle 0.2)
  "
   ivy:   _b_ → mini       _y_ → kill-ring   _l_ → swiper
          _e_ → recentf    _x_ → M-x         _L_ → swiper (multi)
          _f_ → files
"
  ("b" my-ivy-mini)
  ("e" ivy-recentf)
  ("f" counsel-find-file)
  ("y" counsel-yank-pop)
  ("x" counsel-M-x)
  ("l" swiper)
  ("L" swiper-all))



(autoload 'magit-log-popup "magit-log" nil t)
(autoload 'magit-diff-popup "magit-diff" nil t)
(autoload 'magit-commit-popup "magit-commit" nil t)
(autoload 'magit-file-popup "magit" nil t)

(defhydra my-git-hydra (:hint nil :exit t :idle 0.2)
  "
   magit:  _s_ → status  _l_ → log    _f_ → file      staging:  _a_ → +hunk  _A_ → +buffer
           _c_ → commit  _d_ → diff   _z_ → stash               _r_ → -hunk  _R_ → -buffer
           _p_ → push    _b_ → blame  _m_ → merge
"
  ("s" magit-status)
  ("b" magit-blame-popup)
  ("f" magit-file-popup)
  ("z" magit-status-popup)
  ("l" magit-log-popup)
  ("d" magit-diff-popup)
  ("c" magit-commit-popup)
  ("m" magit-merge-popup)
  ("p" magit-push-popup)
  ("a" git-gutter+-stage-hunks)
  ("r" git-gutter+-revert-hunk)
  ("A" git-gutter+-stage-whole-buffer)
  ("R" git-gutter+-unstage-whole-buffer))



(defhydra my-paste-hydra (:hint nil :idle 0.2)
  "
   Paste transient state: [%s(length kill-ring-yank-pointer)/%s(length kill-ring)]
         _C-j_ → cycle next          _p_ → paste before
         _C-k_ → cycle previous      _P_ → paste after
"
  ("C-j" evil-paste-pop-next)
  ("C-k" evil-paste-pop)
  ("p" evil-paste-after)
  ("P" evil-paste-before))



(when (> (length narrow-map) 8)
  (error "`narrow-map' has more than 7 bindings!"))
(defhydra my-narrow-hydra (:hint nil :exit t :idle 0.2)
  "
   narrow  _d_ → defun   _b_ → org-block    _w_ → widen
           _n_ → region  _e_ → org-element
           _p_ → page    _s_ → org-subtree
"
  ("b" org-narrow-to-block)
  ("e" org-narrow-to-element)
  ("s" org-narrow-to-subtree)
  ("d" narrow-to-defun)
  ("n" narrow-to-region)
  ("p" narrow-to-page)
  ("w" widen))

(provide 'init-hydras)
