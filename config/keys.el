(require 'dotemacs-common)
(require 'def-bindings)
(require 'def-utils)

(require-package 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)

(define-leader
  (dotemacs-bindings/leader-key 'execute-extended-command "M-x")
  ("t"   'my-toggle-hydra/body "toggle")
  ("T" 'dotemacs/switch-theme "toggle theme")
  ("q"   'my-quit-hydra/body "quit")
  ("j"   'my-jump-hydra/body "jump")
  ("s"   'my-search-hydra/body "search")
  ("l"   'my-jump-hydra/lambda-l-and-exit "lines(current)")
  ("L"   'my-jump-hydra/lambda-L-and-exit "lines(all)")
  ("o"   'my-jump-hydra/lambda-i-and-exit "outline")
  ("'"   'my-new-eshell-split "shell")
  ("w"   'evil-window-map "windows")
  ("TAB" 'mru-buffer "mru buffer")
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

(dotemacs/describe-leader-key "c" "compilation")
(define-leader
  ("c c" 'compile)
  ("c b" (bind (-some->> (compilation-find-buffer) (switch-to-buffer))) "buffer")
  ("c k" 'kill-compilation))

(after 'projectile
  (dotemacs/describe-leader-key "p" "project")
  (define-leader
    ("p 4 a" #'projectile-find-other-file-other-window)
    ("p 4 b" #'projectile-switch-to-buffer-other-window)
    ("p 4 C-o" #'projectile-display-buffer)
    ("p 4 d" #'projectile-find-dir-other-window)
    ("p 4 f" #'projectile-find-file-other-window)
    ("p 4 g" #'projectile-find-file-dwim-other-window)
    ("p 4 t" #'projectile-find-implementation-or-test-other-window)
    ("p !" #'projectile-run-shell-command-in-root)
    ("p &" #'projectile-run-async-shell-command-in-root)
    ("p a" #'projectile-find-other-file)
    ("p b" #'projectile-switch-to-buffer)
    ("p c" #'projectile-compile-project)
    ("p d" #'projectile-find-dir)
    ("p D" #'projectile-dired)
    ("p e" #'projectile-recentf)
    ("p E" #'projectile-edit-dir-locals)
    ("p f" #'projectile-find-file)
    ("p g" #'projectile-find-file-dwim)
    ("p F" #'projectile-find-file-in-known-projects)
    ("p i" #'projectile-invalidate-cache)
    ("p I" #'projectile-ibuffer)
    ("p j" #'projectile-find-tag)
    ("p k" #'projectile-kill-buffers)
    ("p l" #'projectile-find-file-in-directory)
    ("p m" #'projectile-commander)
    ("p o" #'projectile-multi-occur)
    ("p p" #'projectile-switch-project)
    ("p q" #'projectile-switch-open-project)
    ("p P" #'projectile-test-project)
    ("p r" #'projectile-replace)
    ("p R" #'projectile-regenerate-tags)
    ("p s g" #'projectile-grep)
    ("p s s" #'projectile-ag)
    ("p S" #'projectile-save-project-buffers)
    ("p t" #'neotree-find-project-root "neotree at root")
    ("p T" #'projectile-find-test-file)
    ("p u" #'projectile-run-project)
    ("p v" #'projectile-vc)
    ("p V" #'projectile-browse-dirty-projects)
    ("p x e" #'projectile-run-eshell)
    ("p x t" #'projectile-run-term)
    ("p x s" #'projectile-run-shell)
    ("p z" #'projectile-cache-current-file)))

;; Buffer keys
(dotemacs/describe-leader-key "b" "buffers")
(define-leader
  ("b s" 'my-goto-scratch-buffer "goto scratch")
  ("b d" 'evil-delete-buffer "delete buffer")
  ("b B" 'bury-buffer "bury buffer")
  ("b n" 'next-buffer "next buffer")
  ("b N" 'previous-buffer "previous buffer")
  ("b p" 'previous-buffer "previous buffer")
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

;; Narrowing
(dotemacs/describe-leader-key "n" "narrow")
(define-leader
  ("n n" 'fancy-narrow-to-region)
  ("n d" 'fancy-narrow-to-defun)
  ("n p" 'fancy-narrow-to-page)
  ("n b" 'org-fancy-narrow-to-block)
  ("n e" 'org-fancy-narrow-to-element)
  ("n s" 'org-fancy-narrow-to-subtree)
  ("n w" 'fancy-widen))


;; Select/move windows
(dotimes (i 10)
  (define-leader
    ((format "%i"  i) (intern (format "winum-select-window-%s" i)))
    ((format "b%i" i) (intern (format "buffer-to-window-%s" i)))))

(after "persp-mode-autoloads"
  (define-leader
    ("P" 'persp-key-map "perspective")))

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
    ("g h" #'my-git-staging-hydra/body "pick hunks")
    ("g g" #'global-git-gutter+-mode   "gutter"))


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

(after 'evil
  (evil-define-minor-mode-key 'normal 'hs-minor-mode-map
    "zm" 'hs-hide-level
    "zM" 'evil-close-folds
    ))

(after "projectile-autoloads"
  (define-leader
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


(global-set-key (kbd "C-x C-c") (bind (message "Thou shall not quit!")))
(after 'evil
  (defadvice evil-quit (around dotemacs activate)
    (message "Thou shall not quit!"))
  (defadvice evil-quit-all (around dotemacs activate)
    (message "Thou shall not quit!")))


(provide 'keys)
