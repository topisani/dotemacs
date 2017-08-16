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


(after 'evil
  (require-package 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

  (define-leader
    (dotemacs-bindings/leader-key #'execute-extended-command "M-x")
    ("t" #'my-toggle-hydra/body "toggle...")
    ("q" #'my-quit-hydra/body "quit...")
    ("e" #'my-errors-hydra/body "errors...")
    ("j" #'my-jump-hydra/body "jump...")
    ("f" #'my-file-hydra/body "files...")
    ("b" #'my-buffer-hydra/body "buffers...")
    ("s" #'my-search-hydra/body "search...")
    ("l" #'my-jump-hydra/lambda-l-and-exit "lines(current)")
    ("L" #'my-jump-hydra/lambda-L-and-exit "lines(all)")
    ("o" #'my-jump-hydra/lambda-i-and-exit "outline")
    ("'" #'my-new-eshell-split "shell")
    ("w" #'evil-window-map "windows...")
    ("TAB" #'my-mru-buffer "mru buffer")
    ("n" #'my-narrow-hydra/body "narrow...")
    ("y" (bind
          (cond ((eq dotemacs-switch-engine 'ivy)
                 (call-interactively #'counsel-yank-pop))
                ((eq dotemacs-switch-engine 'helm)
                 (call-interactively #'helm-show-kill-ring)))) "kill-ring"))

  (-define-keys evil-window-map
    ("d" #'evil-window-delete))

  (after "magit-autoloads"
    (autoload 'magit-log-popup "magit-log" nil t)
    (autoload 'magit-diff-popup "magit-diff" nil t)
    (autoload 'magit-commit-popup "magit-commit" nil t)
    (autoload 'magit-file-popup "magit" nil t)
    (define-leader
      ("g s" #'magit-status "status")
      ("g b" #'magit-blame-popup "blame")
      ("g f" #'magit-file-popup "file")
      ("g z" #'magit-status-popup "stash")
      ("g l" #'magit-log-popup "log")
      ("g d" #'magit-diff-popup "diff")
      ("g c" #'magit-commit-popup "commit")
      ("g m" #'magit-merge-popup "merge")
      ("g p" #'magit-push-popup "push")
      ("g h" #'my-git-staging-hydra/body "pick hunks"))

    (after 'with-editor
      (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
      (let ((mm-key dotemacs-bindings/major-key))
        (define-evilified-keys with-editor-mode-map
          ((concat mm-key mm-key) 'with-editor-finish "finish")
          ((concat mm-key "a")    'with-editor-cancel "cancel")
          ((concat mm-key "c")    'with-editor-finish "finish")
          ((concat mm-key "k")    'with-editor-cancel "cancel")))))

  (after "helm-autoloads"
    (define-leader ("h" #'my-helm-hydra/body "helm...")))

  (after "evil-numbers-autoloads"
    (-define-key evil-normal-state-map "C-a" #'evil-numbers/inc-at-pt)
    (-define-key evil-normal-state-map "C-S-a" #'evil-numbers/dec-at-pt))

  (global-set-key (kbd "C-w") 'evil-window-map)

  ;; emacs lisp
  (evil-define-key 'normal emacs-lisp-mode-map "K" (bind (help-xref-interned (symbol-at-point))))
  (after "elisp-slime-nav-autoloads"
    (evil-define-key 'normal emacs-lisp-mode-map (kbd "g d") 'elisp-slime-nav-find-elisp-thing-at-point))

  (after "projectile-autoloads"
    (define-leader
      ("p" #'projectile-command-map "projectile...")
      ("/"
       (bind
        (if current-prefix-arg
            (cond
             ((executable-find "ag")  (call-interactively #'projectile-ag))
             ((executable-find "pt")  (call-interactively #'projectile-pt))
             ((executable-find "ack") (call-interactively #'projectile-ack))
             (t                       (call-interactively #'projectile-grep)))
          (cond
           ((eq dotemacs-switch-engine 'ivy)
            (cond
             ((executable-find "ag") (counsel-ag))
             ((executable-find "pt") (counsel-pt))))
           ((eq dotemacs-switch-engine 'helm)
            (helm-do-ag (projectile-project-root))))))
       "search...")))

  (after "multiple-cursors-autoloads"
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-unset-key (kbd "M-<down-mouse-1>"))
    (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
    (evil-global-set-key 'normal "g r" 'mc/mark-all-like-this-dwim))

  (after 'js2-mode
    (evil-define-key 'normal js2-mode-map (kbd "g r") #'js2r-rename-var))

  (after "avy-autoloads"
    (define-key evil-operator-state-map (kbd "z") 'avy-goto-char-2)
    (define-key evil-normal-state-map (kbd "s") 'avy-goto-char-2)
    (define-key evil-motion-state-map (kbd "S-SPC") 'avy-goto-line))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-h") #'evil-window-left)
              (local-set-key (kbd "C-j") #'evil-window-down)
              (local-set-key (kbd "C-k") #'evil-window-up)
              (local-set-key (kbd "C-l") #'evil-window-right))))

;; escape minibuffer
(define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

(define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word)

(after 'ivy
  (define-key ivy-mode-map [escape] (kbd "C-g")))

(after "neotree-autoloads"
  (after 'neotree
    (evilified-state--evilified-state-on-entry)
    (define-evilified-keys neotree-mode-map
      ("RET" 'neotree-enter "open")
      ("TAB" 'neotree-stretch-toggle "shring/enlarge")
      ("|" 'neotree-enter-vertical-split "vertical split")
      ("-" 'neotree-enter-horizontal-split "horizontal split")
      ("'" 'neotree-quick-look "quick look")
      ("c" 'neotree-create-node "create")
      ("C" 'neotree-copy-node "copy")
      ("d" 'neotree-delete-node "delete")
      ;; ("gr" ' neotree-refresh "refresh")
      ("h" 'spacemacs/neotree-collapse-or-up "up/collapse")
      ("H" 'neotree-select-previous-sibling-node "previous sibling")
      ("j" 'neotree-next-line "line down")
      ("J" 'neotree-select-down-node "goto child")
      ("k" 'neotree-previous-line "line up")
      ("K" 'neotree-select-up-node "goto parent")
      ("l" 'spacemacs/neotree-expand-or-open "open/expand")
      ("L" 'neotree-select-next-sibling-node "next sibling")
      ("r" 'neotree-rename-node "rename")
      ("R" 'neotree-change-root "change root")
      ("s" 'neotree-hidden-file-toggle '(if neo-buffer--show-hidden-file-p "[x] hidden files" "[ ] hidden files")))

    (define-leader
      ;; ("ft" 'neotree-toggle)
      ;; ("fT" 'neotree-show)
      ;; ("pt" 'neotree-find-project-root)
      )))

(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))


(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))


(after "expand-region-autoloads"
  (global-set-key (kbd "C-=") 'er/expand-region))


;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))


(after 'compile
  (define-key compilation-mode-map (kbd "j") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "k") 'compilation-previous-error))


(after "helm-autoloads"
  (-define-key (current-global-map) "C-c h" #'my-helm-hydra/body "helm..."))


(after "counsel-autoloads"
  (define-leader ("i" #'my-ivy-hydra/body "ivy...")))


(global-set-key [prior] 'previous-buffer)
(global-set-key [next] 'next-buffer)

(-define-keys (current-global-map)
  ("C-c c" #'org-capture)
  ("C-c a" #'org-agenda)
  ("C-c l" #'org-store-link)
  ("C-c s" #'my-goto-scratch-buffer)
  ("C-c e" #'my-eval-and-replace)
  ("C-c t" #'my-new-eshell-split))

(-define-keys (current-global-map)
  ("C-x c" #'calculator)
  ("C-x C" #'calendar)
  ("C-x n" #'my-narrow-hydra/body)
  ("C-x p" #'proced))

;; have no use for these default bindings
(global-unset-key (kbd "C-x m"))


(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(after 'evil
  (defadvice evil-quit (around dotemacs activate)
    (message "Thou shall not quit!"))
  (defadvice evil-quit-all (around dotemacs activate)
    (message "Thou shall not quit!")))


(provide 'init-bindings)
