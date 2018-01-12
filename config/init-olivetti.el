(require 'dotemacs-common)

(require-package 'olivetti)

(setq-default olivetti-body-width 100)

(add-named-hook 'olivetti-mode-hook
  (nlinum-mode -1))

(provide 'init-olivetti)
