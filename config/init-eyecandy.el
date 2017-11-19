(defgroup dotemacs-eyecandy nil
  "Configuration options for eye candy."
  :group 'dotemacs
  :prefix 'dotemacs-eyecandy)

(defcustom dotemacs-eyecandy/mode-line
  'spaceline
  "List of hooks to automatically start up in Evil Emacs state."
  :type '(radio
          (const :tag "smart mode line" sml)
          (const :tag "telephone line" telephone-line)
          (const :tag "spaceline" spaceline))
  :group 'dotemacs-eyecandy)



(when (eq dotemacs-pair-engine 'emacs)
  (show-paren-mode)
  (setq show-paren-delay 0))


(require-package 'nlinum)
(column-number-mode t)
(global-nlinum-mode t)
(display-time-mode t)
(size-indication-mode t)

(defun my-fold-overlay (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((count (count-lines (overlay-start ov) (overlay-end ov))))
      (overlay-put ov 'after-string
                   (format "" count)))))

(setq hs-set-up-overlay 'my-fold-overlay)
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; (require-package 'evil-vimish-fold)
;; (vimish-fold-global-mode 1)
;; (evil-vimish-fold-mode 1)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(require-package 'diminish)
(diminish 'visual-line-mode)
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'auto-complete (diminish 'auto-complete-mode "a"))
(after 'company (diminish 'company-mode "C"))
(after 'projectile (diminish 'projectile-mode ""))
(after 'yasnippet (diminish 'yas-minor-mode "Y"))
(after 'which-key (diminish 'which-key-mode))
(after 'eldoc (diminish 'eldoc-mode "e"))
(after 'smartparens (diminish 'smartparens-mode "p"))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'highlight-symbol (diminish 'highlight-symbol-mode "H"))
(after 'indent-guide (diminish 'indent-guide-mode "I"))
(after 'hideshow (diminish 'hs-minor-mode))
(after 'ivy (diminish 'ivy-mode))
(after 'helm-mode (diminish 'helm-mode))
(after 'evil-commentary (diminish 'evil-commentary-mode "c"))
(after 'page-break-lines (diminish 'page-break-lines-mode ))
(after 'flycheck (diminish 'flycheck-mode "s"))
(after 'flyspell (diminish 'flyspell-mode "S"))
(after 'aggressive-indent (diminish 'aggressive-indent-mode))
(after 'counsel (diminish #'counsel-mode))
(after 'window-purpose (diminish 'purpose-mode))


(defun dotemacs/setup-modeline ()
  (pcase dotemacs-eyecandy/mode-line
    ('sml (progn
            (require-package 'smart-mode-line)
            (sml/setup)
            (after 'evil
              (defvar dotemacs--original-mode-line-bg (face-background 'mode-line))
              (defadvice evil-set-cursor-color (after dotemacs activate)
                (cond ((evil-emacs-state-p)
                       (set-face-background 'mode-line "#440000"))
                      ((evil-insert-state-p)
                       (set-face-background 'mode-line "#002244"))
                      ((evil-visual-state-p)
                       (set-face-background 'mode-line "#440044"))
                      (t
                       (set-face-background 'mode-line dotemacs--original-mode-line-bg)))))))
    ('spaceline (progn
                  (require-package 'spaceline)
                  (require 'spaceline-config)
                  (setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
                  (dotemacs-spaceline/setup)
                  (after "helm-autoloads"
                    (spaceline-helm-mode))))
    ('telephone-line (after 'all-the-icons
                       (telephone-line-mode 1)))))


(dotemacs/setup-modeline)


(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode)
  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . 955) prettify-symbols-alist)
              (push '("return" . 8592) prettify-symbols-alist))))

(delayed-init
 (require-package 'color-identifiers-mode)
 (global-color-identifiers-mode -1)
 (diminish 'color-identifiers-mode))


(require-package 'all-the-icons)

(require-package 'fancy-narrow)
(fancy-narrow-mode)


(require-package 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.3)
;; (add-hook 'prog-mode-hook 'highlight-symbol-mode)


(require-package 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)


(require-package 'highlight-quoted)
(add-hook 'prog-mode-hook 'highlight-quoted-mode)


(require-package 'page-break-lines)
(global-page-break-lines-mode)


(add-hook 'find-file-hook 'hl-line-mode)
(require-package 'fill-column-indicator)


;; Theme packages
(require-package 'gruvbox-theme)


(provide 'init-eyecandy)
