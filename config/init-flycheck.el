(require 'dotemacs-common)

(require-package 'flycheck)

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-standard-error-navigation nil)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc html-tidy))

(after 'web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(when (display-graphic-p)
  (require-package 'flycheck-pos-tip)
  (flycheck-pos-tip-mode))

(evilify flycheck-error-list-mode flycheck-error-list-mode-map)

(after 'evil
  (evil-declare-ignore-repeat 'spacemacs/next-error)
  (evil-declare-ignore-repeat 'spacemacs/previous-error))

;; Disable fringe markers
(setq flycheck-indication-mode nil)
(setq flycheck-emacs-lisp-load-path 'inherit)

(provide 'init-flycheck)
