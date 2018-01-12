(defgroup dotemacs nil
  "Custom configuration for dotemacs."
  :group 'local)

(defcustom dotemacs-cache-directory (concat user-emacs-directory ".cache/")
  "The storage location for various persistent files."
  :type 'directory
  :group 'dotemacs)

(defcustom dotemacs-completion-engine
  'company
  "The completion engine the use."
  :type '(radio
          (const :tag "company-mode" company)
          (const :tag "auto-complete-mode" auto-complete))
  :group 'dotemacs)

(defcustom dotemacs-switch-engine
  'helm
  "The primary engine to use for narrowing and navigation."
  :type '(radio
          (const :tag "helm" helm)
          (const :tag "ido" ido)
          (const :tag "ivy" ivy))
  :group 'dotemacs)

(defcustom dotemacs-pair-engine
  'emacs
  "The primary engine to use auto-pairing and parens matching."
  :type '(radio
          (const :tag "emacs" emacs)
          (const :tag "smartparens" smartparens))
  :group 'dotemacs)

(defvar dotemacs-config-directory (concat user-emacs-directory "config"))



(defgroup dotemacs-bindings nil
  "Configuration options for general key bindings"
  :group 'dotemacs
  :prefix 'dotemacs-bindings)

(defcustom dotemacs-bindings/major-key
  ","
  "The prefix key for major-mode specific bindings.
   Remaps to `<leader> m'"
  :type 'key-sequence
  :group 'dotemacs-bindings)

(defcustom dotemacs-bindings/leader-key
  "SPC"
  "The prefix key for general leader bindings."
  :type 'key-sequence
  :group 'dotemacs-bindings)

(defcustom dotemacs-bindings/fallback-major-key
  "C-,"
  "fallback `<major-key>'"
  :type 'key-sequence
  :group 'dotemacs-bindings)

(defcustom dotemacs-bindings/fallback-leader-key
  "C-SPC"
  "Fallback `leader-key'"
  :type 'key-sequence
  :group 'dotemacs-bindings)



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
    xref--xref-buffer-mode
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

(setq evil-want-C-u-scroll dotemacs-evil/evil-want-C-u-scroll)

(provide 'dotemacs-customs)
