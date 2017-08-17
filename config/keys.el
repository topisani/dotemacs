(require-package 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

(define-leader
  (dotemacs-bindings/leader-key 'execute-extended-command "M-x")
  ("t"   'my-toggle-hydra/body "toggle")
  ("q"   'my-quit-hydra/body "quit")
  ("j"   'my-jump-hydra/body "jump")
  ("s"   'my-search-hydra/body "search")
  ("l"   'my-jump-hydra/lambda-l-and-exit "lines(current)")
  ("L"   'my-jump-hydra/lambda-L-and-exit "lines(all)")
  ("o"   'my-jump-hydra/lambda-i-and-exit "outline")
  ("'"   'my-new-eshell-split "shell")
  ("w"   'evil-window-map "windows")
  ("TAB" 'my-mru-buffer "mru buffer")
  ("n"   'my-narrow-hydra/body "narrow")
  ("y"   (bind (my-switch-action #'yank-pop :ivy 'counsel-yank-pop :helm 'helm-show-kill-ring)) "kill-ring"))

(-define-keys evil-window-map
  ("f" 'make-frame "new frame")
  ("d" 'evil-window-delete "close window"))

;; File keys
(dotemacs/describe-leader-key "f" "files")
(define-leader
  ("f D" 'my-delete-current-buffer-file "delete")
  ("f R" 'my-rename-current-buffer-file "rename")
  ("f f" (bind (my-switch-action #'find-file :ivy #'counsel-find-file :helm #'helm-find-files)) "find files")
  ("f r" (bind (my-switch-action #'recentf   :ivy #'ivy-recentf       :helm #'helm-recentf)) "recent files")
  ("f y" 'my-copy-file-name-to-clipboard "copy filename")
  ("f E" 'my-find-file-as-root "edit as root")
  ("f c" 'copy-file "copy file")
  ("f t" 'dotemacs/neotree-toggle "toggle neotree")
  ("f C" 'my-file-convert-hydra/body "convert")
  ("f s" 'save-buffer "save")
  ("f d" 'dotemacs/show-config-files "config files"))

;; Buffer keys
(dotemacs/describe-leader-key "b" "buffers")
(define-leader
  ("b s" 'my-goto-scratch-buffer "goto scratch")
  ("b d" 'kill-this-buffer "delete buffer")
  ("b n" 'next-buffer "next buffer")
  ("b N" 'next-buffer "previous buffer")
  ("b m" (bind (switch-to-buffer "*Messages*")) "goto messages")
  ("b b" (bind (my-switch-action #'switch-to-buffer :ivy #'my-ivy-mini :helm #'helm-mini)) "buffers")
  ("b e" 'erase-buffer "erase buffer")
  ("b E" (bind (let ((inhibit-read-only t)) (erase-buffer))) "erase buffer (force)"))

;; Flycheck keys
(dotemacs/describe-leader-key "e" "errors")
(define-leader
  ("e l" 'flycheck-list-errors "list errors")
  ("e n" 'flycheck-next-error "next error")
  ("e N" 'flycheck-previous-error "prev error")
  ("e p" 'flycheck-previous-error "prev error")
  ("e v" 'flycheck-verify-setup "verify setup")
  ("e ?" 'flycheck-describe-checker "describe checker"))

;; Select/move windows
(dotimes (i 10)
  (define-leader
    ((format "%i"  i) (intern (format "winum-select-window-%s" i)))
    ((format "b%i" i) (intern (format "buffer-to-window-%s" i)))))

;; Magit keys
(after "magit-autoloads"
  (autoload 'magit-log-popup "magit-log" nil t)
  (autoload 'magit-diff-popup "magit-diff" nil t)
  (autoload 'magit-commit-popup "magit-commit" nil t)
  (autoload 'magit-file-popup "magit" nil t)
  (define-leader
    ("g s" #'magit-status              "status"    )
    ("g b" #'magit-blame-popup         "blame"     )
    ("g f" #'magit-file-popup          "file"      )
    ("g z" #'magit-status-popup        "stash"     )
    ("g l" #'magit-log-popup           "log"       )
    ("g d" #'magit-diff-popup          "diff"      )
    ("g c" #'magit-commit-popup        "commit"    )
    ("g m" #'magit-merge-popup         "merge"     )
    ("g p" #'magit-push-popup          "push"      )
    ("g h" #'my-git-staging-hydra/body "pick hunks"))

  (after 'with-editor
    (add-hook 'with-editor-mode-hook 'evil-normalize-keymaps)
    (let ((mm-key dotemacs-bindings/major-key))
      (define-evilified-keys with-editor-mode-map
        ((concat mm-key mm-key) 'with-editor-finish "finish")
        ((concat mm-key "a")    'with-editor-cancel "cancel")
        ((concat mm-key "c")    'with-editor-finish "finish")
        ((concat mm-key "k")    'with-editor-cancel "cancel")))))

;; Evil addons
(after "evil-numbers-autoloads"
  (-define-key evil-normal-state-map "C-a" #'evil-numbers/inc-at-pt)
  (-define-key evil-normal-state-map "C-S-a" #'evil-numbers/dec-at-pt))

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
            (local-set-key (kbd "C-l") #'evil-window-right)))

;; Comint
(after 'comint
  (define-key comint-mode-map [up] 'comint-previous-input)
  (define-key comint-mode-map [down] 'comint-next-input))

;; Company
(after 'company
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "<tab>") 'my-company-tab)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

;; mouse scrolling in terminal
(unless (display-graphic-p)
  (global-set-key [mouse-4] (bind (scroll-down 1)))
  (global-set-key [mouse-5] (bind (scroll-up 1))))

;; Compilation mode
(after 'compile
  (define-key compilation-mode-map (kbd "j") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "k") 'compilation-previous-error))

(-define-keys (current-global-map)
  ("C-c c" #'org-capture)
  ("C-c a" #'org-agenda)
  ("C-c l" #'org-store-link)
  ("C-c t" #'my-new-eshell-split))

;; Applications
(dotemacs/describe-leader-key "a" "apps")
(define-leader
  ("a c" #'calculator)
  ("a C" #'calendar)
  ("a p" #'proced))

;; Escape from various states
(progn
  (after 'ivy
    (define-key ivy-mode-map [escape] (kbd "C-g")))
  (define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'my-minibuffer-keyboard-quit)

  (define-key minibuffer-local-map (kbd "C-w") 'backward-kill-word))

;; Disable default keybindings
(global-unset-key (kbd "C-x m"))

;; 
(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(after 'evil
  (defadvice evil-quit (around dotemacs activate)
    (message "Thou shall not quit!"))
  (defadvice evil-quit-all (around dotemacs activate)
    (message "Thou shall not quit!")))


(provide 'init-bindings)
