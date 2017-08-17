(defgroup dotemacs-evil nil
  "Configuration options for evil-mode."
  :group 'dotemacs
  :prefix 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-hooks
  '(org-log-buffer-setup-hook org-capture-mode-hook)
  "List of hooks to automatically start up in Evil Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-major-modes
  '(eshell-mode
    term-mode
    calculator-mode
    makey-key-mode)
  "List of major modes that should default to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-state-minor-modes
  '(edebug-mode
    git-commit-mode
    magit-blame-mode)
  "List of minor modes that when active should switch to Emacs state."
  :type '(repeat (symbol))
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/emacs-insert-mode
  nil
  "If non-nil, insert mode will act as Emacs state."
  :type 'boolean
  :group 'dotemacs-evil)

(defcustom dotemacs-evil/evil-want-C-u-scroll
  t
  "If non-nil, `C-u' will scroll up"
  :type 'boolean
  :group 'dotemacs-evil)



(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)

(setq evil-emacs-state-cursor '("red" box))
(setq evil-motion-state-cursor '("orange" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(add-hook 'evil-jumps-post-jump-hook #'recenter)

(require-package 'evil)

(setq evil-want-C-u-scroll dotemacs-evil/evil-want-C-u-scroll)

(require 'evil)
(evil-mode)

(cl-loop for mode in dotemacs-evil/emacs-state-minor-modes
         do (let ((hook (concat (symbol-name mode) "-hook")))
              (add-hook (intern hook) `(lambda ()
                                         (if ,mode
                                             (evil-emacs-state)
                                           (evil-normal-state))))))

(cl-loop for hook in dotemacs-evil/emacs-state-hooks
         do (add-hook hook #'evil-emacs-state))

(cl-loop for mode in dotemacs-evil/emacs-state-major-modes
         do (evil-set-initial-state mode 'emacs))

(add-hook 'after-init-hook
          (lambda ()
            (evil-put-property 'evil-state-properties 'normal   :tag " N ")
            (evil-put-property 'evil-state-properties 'insert   :tag " I ")
            (evil-put-property 'evil-state-properties 'visual   :tag " V ")
            (evil-put-property 'evil-state-properties 'motion   :tag " M ")
            (evil-put-property 'evil-state-properties 'emacs    :tag " E ")
            (evil-put-property 'evil-state-properties 'replace  :tag " R ")
            (evil-put-property 'evil-state-properties 'operator :tag " O ")))



(when dotemacs-evil/emacs-insert-mode
  (defalias 'evil-insert-state 'evil-emacs-state)
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state))


(unless (display-graphic-p)
  (evil-esc-mode))


(require-package 'evil-commentary)
(evil-commentary-mode t)


(require-package 'evil-surround)
(global-evil-surround-mode t)


(require-package 'evil-exchange)
(evil-exchange-install)


(require-package 'evil-anzu)
(require 'evil-anzu)
(setq anzu-cons-mode-line-p nil)


(require-package 'evil-ediff)
(evil-ediff-init)


(after 'magit
  (require-package 'evil-magit)
  (require 'evil-magit))


(require-package 'evil-avy)
(evil-avy-mode)


(require-package 'evil-matchit)
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items))
(global-evil-matchit-mode t)


(require-package 'evil-indent-textobject)
(require 'evil-indent-textobject)


(require-package 'evil-visualstar)
(global-evil-visualstar-mode t)


(require-package 'evil-numbers)

(require-package 'evil-lion)
(evil-lion-mode)

(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\e]50;CursorShape=1\x7")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))
(add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))
(my-evil-terminal-cursor-change)


(defadvice evil-ex-search-next (after dotemacs activate)
  (recenter))

(defadvice evil-ex-search-previous (after dotemacs activate)
  (recenter))



(with-current-buffer "*Messages*"
  (evil-evilified-state))

(provide 'init-evil)
